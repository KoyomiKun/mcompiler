#![allow(dead_code)]
/// support utf-8
use std::{
    collections::HashSet,
    fmt::Write,
    fs::{File, OpenOptions},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Result};
use log::{debug, error};

// get the last char and write to buffer and delete it if condition satisfied
macro_rules! lex_getc_if {
    ($parser: expr, $cond: expr, $buf: expr) => {
        while let Some(c) = $parser.peek_char() {
            if !$cond(c) {
                break;
            }
            $buf.write_char(c)?;
            $parser.next_char();
        }
    };
}

macro_rules! make_token {
    ($self:ident . $func:ident ($parser:ident $(, $params:expr)*) as $token_type: expr) => {
        match $self.$func($parser, $( $params ),*) {
            Err(e) => {
                error!(
                    "make {} token failed: {}",
                    $token_type,
                    $parser.error_string(e)
                );
                return None;
            }
            Ok(t) => {
                return Some(t);
            }
        }
    };
}

const KEYWORDS: [&str; 31] = [
    "unsigned",
    "signed",
    "char",
    "short",
    "int",
    "float",
    "double",
    "long",
    "void",
    "struct",
    "union",
    "static",
    "__ignore_typecheck__",
    "return",
    "include",
    "sizeof",
    "if",
    "else",
    "while",
    "for",
    "do",
    "break",
    "continue",
    "switch",
    "case",
    "default",
    "goto",
    "typedef",
    "const",
    "extern",
    "restrict",
];

// operator 指的是将多个值进行运算并返回一个新值的符号
const OPERATORS_VALID: [&str; 39] = [
    "+", "-", "*", "/", "%", "++", "--", // 算术运算符
    "!=", "==", ">", "<", ">=", "<=", // 关系运算符
    "||", "&&", "!", // 逻辑运算符
    "^", ">>", "<<", "|", "&", "~", // 位运算符
    "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|=", // 赋值运算符
    "?",  // 三元运算符
    "->", "(", "[", ",", ".", // * 也可以是地址操作符；不支持 & 取地址
];

const SYMBOLS: [char; 8] = ['{', '}', ':', ';', '#', '\\', ')', ']'];

#[derive(Debug, PartialEq)]
enum TokenTypeAndValue {
    /// keywords represent datatypes, and control procedure commands such as "break" or "continue"
    Keyword(String),
    /// used for all words with letters under scrolls and numbers that are not keywords
    Identifier(String),
    /// used in expressions such as 5 + 10, "+" would be the operator
    Operator(String),
    /// quotes, semicolons, colons and more; it means some action start or done.
    Symbol(char),
    /// a numerical number
    Number(Num),
    /// a string with data between double quotes
    Str(String),
    /// a c-lang comment
    Comment(String),
    /// a new line in our program
    Newline,
}

#[derive(Debug, PartialEq)]
enum Num {
    UInt(u32),
    ULongInt(u32),
    ULongLongInt(u64),
}

#[derive(Debug, PartialEq, Clone)]
struct Pos {
    line: usize,
    col: usize,
    filename: PathBuf,
}

#[derive(Debug, PartialEq)]
struct Token {
    tv: TokenTypeAndValue,
    pos: Pos,

    /// a flag for tailing " "
    whitespace_tail: bool,
}

trait LexParser {
    fn next_char(&mut self) -> Option<char>;
    fn peek_char(&mut self) -> Option<char>;
    fn push_char(&mut self, c: char);

    fn push_token(&mut self, t: Token);
    fn get_tokens(&self) -> &Vec<Token>;
    fn get_last_token(&self) -> Option<&Token>;
    fn get_last_token_mut(&mut self) -> Option<&mut Token>;

    fn error_string(&self, err: anyhow::Error) -> String;
    fn error(&self, s: &str) -> anyhow::Error;

    // expression is content in parentheses
    fn new_expression(&mut self);
    fn end_expression(&mut self) -> Result<()>;
    fn is_in_expression(&self) -> bool;

    fn copy_position(&self) -> Pos;
}

struct Lexer<'a> {
    keywords: HashSet<&'a str>,
    /// operators can be combined such as "<<" or ">>"
    // technically brackets are operator, but when we found a symbol ')' we known operator '('
    // done. that's why we put ')' in symbols
    operators_lead_char: HashSet<char>,
    operators_following_chars: HashSet<char>,
    operators_valid: HashSet<&'a str>,
    symbols: HashSet<char>,
}

struct FileLexer {
    input_reader: BufReader<File>,
    // current_line contains '\n'
    current_line: String,
    output_file: File,

    pos: Pos,

    current_expression_count: isize,
    parentheses_buffer: String,

    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new() -> Self {
        // operators' lead chars, such as '+' to '+='
        let operators_lead_char = OPERATORS_VALID
            .into_iter()
            .map(|s| s.chars().next().expect("BUG: str in operators is empty"))
            .collect::<HashSet<char>>();

        let operators_following_chars = OPERATORS_VALID
            .into_iter()
            .flat_map(|x| x.chars().skip(1))
            .collect::<HashSet<char>>();

        Self {
            keywords: HashSet::from(KEYWORDS),

            operators_valid: HashSet::from(OPERATORS_VALID),
            operators_lead_char,
            operators_following_chars,

            symbols: HashSet::from(SYMBOLS),
        }
    }

    fn parse<T: LexParser>(&self, parser: &'a mut T) -> &Vec<Token> {
        while let Some(t) = self.next_token(parser) {
            parser.push_token(t);
        }
        parser.get_tokens()
    }

    fn next_token<T: LexParser>(&self, parser: &mut T) -> Option<Token> {
        if let Some(c) = parser.peek_char() {
            debug!("peek char {c}");
            if Self::is_digit(c) {
                make_token!(self.make_num_token(parser) as "num")
            }

            // judge \n first or it will be recognized as a whitespace
            if c == '\n' {
                make_token!(self.make_newline_token(parser) as "newline")
            }
            if c.is_whitespace() {
                return self.handle_whitespace(parser);
            }

            if c == '"' {
                make_token!(self.make_string_token(parser, '"', '"') as "string")
            }

            if self.operators_lead_char.contains(&c) {
                make_token!(
                    self.make_operator_or_string_token(parser) as
                    "operator or string"
                )
            }

            if self.symbols.contains(&c) {
                make_token!(self.make_symbol_token(parser) as "symbol")
            }

            if c == '_' || c.is_ascii_alphabetic() {
                make_token!(self.make_identifier_or_keyword_token(parser) as "identifier or keyword")
            }

            error!("{}", parser.error("unexpected token"));
        }

        None
    }

    fn make_newline_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let p = parser.copy_position();
        parser.next_char();
        Ok(Token {
            tv: TokenTypeAndValue::Newline,
            pos: p,
            whitespace_tail: false,
        })
    }

    fn make_identifier_or_keyword_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let p = parser.copy_position();
        let mut buffer = String::new();

        lex_getc_if!(parser, Self::is_identifier, buffer);

        // if is keyword
        if self.keywords.contains(buffer.as_str()) {
            return Ok(Token {
                tv: TokenTypeAndValue::Keyword(buffer),
                pos: p,
                whitespace_tail: false,
            });
        }

        Ok(Token {
            tv: TokenTypeAndValue::Identifier(buffer),
            pos: p,
            whitespace_tail: false,
        })
    }

    fn make_symbol_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let p = parser.copy_position();
        let c = parser
            .next_char()
            .expect("BUG: make symbol token while parsed content is empty");
        if c == ')' {
            parser
                .end_expression()
                .map_err(|e| anyhow!(parser.error_string(e)))?;
        }

        Ok(Token {
            tv: TokenTypeAndValue::Symbol(c),
            pos: p,
            whitespace_tail: false,
        })
    }

    // parse 42314
    fn make_num_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let mut buf = String::new();
        let p = parser.copy_position();
        lex_getc_if!(parser, Self::is_digit, buf);

        Ok(Token {
            tv: TokenTypeAndValue::Number(Num::ULongLongInt(buf.parse()?)),
            pos: p,
            whitespace_tail: false,
        })
    }

    fn make_string_token<T: LexParser>(
        &self,
        parser: &mut T,
        start_delim: char,
        end_delim: char,
    ) -> Result<Token> {
        assert_eq!(Some(start_delim), parser.next_char());
        let mut buf = String::new();
        let p = parser.copy_position();
        while let Some(c) = parser.peek_char() {
            if c == end_delim {
                break;
            }
            // ignore escape
            if c == '\\' {
                continue;
            }
            buf.write_char(c)?;
            parser.next_char();
        }
        assert_eq!(Some(end_delim), parser.next_char());

        Ok(Token {
            tv: TokenTypeAndValue::Str(buf),
            pos: p,
            whitespace_tail: false,
        })
    }

    // OR STRING means condition like #include <stdio.h>; we treat stdio.h as a string
    fn make_operator_or_string_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let pos = parser.copy_position();
        let op = self.read_op(parser)?;
        if op == "<" {
            if let Some(t) = parser.get_last_token() {
                if Self::is_keyword(&t.tv, "include") && t.whitespace_tail == true {
                    return self.make_string_token(parser, '<', '>');
                }
            }
        }

        // TODO: finish expression
        if op == "(" {
            parser.new_expression();
        }

        Ok(Token {
            tv: TokenTypeAndValue::Operator(op),
            pos,
            whitespace_tail: false,
        })
    }

    fn read_op<T: LexParser>(&self, parser: &mut T) -> Result<String> {
        let mut buf = String::new();

        // check first char
        let op = parser
            .next_char()
            .expect("BUG: call read operator while encountering EOF");
        buf.write_char(op)?;

        // check following chars
        while let Some(op) = parser.peek_char() {
            if !self.operators_following_chars.contains(&op) {
                break;
            }

            buf.write_char(op)?;
            parser.next_char();
        }

        // push back the elems until operator is valid
        // 最长匹配法
        // 找到那个valid的operator的buf[:i], 使得i最大
        let mut buf_str = buf.as_str();
        // last值是最后一个被push回去的char的index
        if let Some(i) = buf
            .chars()
            .rev()
            .enumerate()
            .take_while(|(i, _)| !self.operators_valid.contains(&buf_str[..buf_str.len() - i]))
            .map(|(i, c)| {
                parser.push_char(c);
                buf_str.len() - i - 1
            })
            .last()
        {
            // i 是 1 说明 整个buf都被push回去了, 还是没找到合理的op
            if i == 0 {
                return Err(parser.error("invalid operator"));
            }
            // i 是其他 表示有部分不满足被push回去了，但buf[:i]之间是满足的
            buf_str = &buf_str[..i];
        }
        return Ok(buf_str.to_string());
    }

    fn handle_whitespace<T: LexParser>(&self, parser: &mut T) -> Option<Token> {
        parser.get_last_token_mut().map(|t| {
            t.whitespace_tail = true;
        });

        parser.next_char();
        while let Some(c) = parser.peek_char() {
            if !c.is_whitespace() {
                break;
            }
            parser.next_char();
        }

        return self.next_token(parser);
    }

    fn is_keyword(token: &TokenTypeAndValue, content: &str) -> bool {
        token == &TokenTypeAndValue::Keyword(content.to_string())
    }

    fn is_digit(c: char) -> bool {
        ('0'..='9').contains(&c)
    }

    fn is_identifier(c: char) -> bool {
        ('A'..='Z').contains(&c) || ('a'..='z').contains(&c) || '_' == c || ('0'..='9').contains(&c)
    }
}

impl FileLexer {
    pub fn new(filename: &Path, filename_out: &Path) -> Result<Self> {
        let f = OpenOptions::new().read(true).open(filename)?;
        let of = OpenOptions::new().write(true).open(filename_out)?;
        let b = BufReader::new(f);
        Ok(Self {
            input_reader: b,
            current_line: String::new(),
            output_file: of,

            pos: Pos {
                line: 0,
                col: 0,
                filename: filename.to_path_buf(),
            },

            current_expression_count: 0,
            parentheses_buffer: String::new(),

            tokens: Vec::new(),
        })
    }

    // return false if EOF, else true
    fn check_line_or_read(&mut self) -> bool {
        if self.current_line.is_empty() {
            let mut buf = String::new();
            match self.input_reader.read_line(&mut buf) {
                Ok(n) if n == 0 => {
                    return false;
                }
                Err(e) => {
                    panic!("read input file failed: {e}")
                }
                _ => {
                    self.current_line = buf.chars().rev().collect();
                }
            };
        }
        true
    }
}

impl LexParser for FileLexer {
    fn next_char(&mut self) -> Option<char> {
        if !self.check_line_or_read() {
            return None;
        }

        match self.current_line.pop() {
            Some(c) if c == '\n' => {
                self.pos.line += 1;
                self.pos.col = 0;
                Some('\n')
            }
            Some(c) => {
                self.pos.col += 1;
                Some(c)
            }
            None => {
                panic!("BUG: pop a empty string, it should be returned before")
            }
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        if !self.check_line_or_read() {
            return None;
        }
        self.current_line.chars().last()
    }

    fn push_char(&mut self, c: char) {
        self.current_line.push(c)
    }

    fn copy_position(&self) -> Pos {
        self.pos.clone()
    }

    fn error_string(&self, err: anyhow::Error) -> String {
        format!(
            "on line {}, col {} in file {}, {}",
            self.pos.line,
            self.pos.col,
            self.pos.filename.to_string_lossy().to_string(),
            err,
        )
    }

    fn error(&self, s: &str) -> anyhow::Error {
        anyhow!(format!(
            "on line {}, col {} in file {}, {}",
            self.pos.line,
            self.pos.col,
            self.pos.filename.to_string_lossy().to_string(),
            s,
        ))
    }
    fn push_token(&mut self, t: Token) {
        self.tokens.push(t)
    }

    fn get_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    fn get_last_token(&self) -> Option<&Token> {
        self.tokens.last()
    }

    fn get_last_token_mut(&mut self) -> Option<&mut Token> {
        self.tokens.last_mut()
    }

    fn new_expression(&mut self) {
        self.current_expression_count += 1;
        if self.current_expression_count == 1 {
            self.parentheses_buffer.clear();
        }
    }

    fn end_expression(&mut self) -> Result<()> {
        self.current_expression_count -= 1;
        if self.current_expression_count < 0 {
            return Err(anyhow!("you close an expression which you never opened"));
        }
        Ok(())
    }

    fn is_in_expression(&self) -> bool {
        self.current_expression_count > 0
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn compile_char_test() {
        let mut l = create_temp_filelexer("H\ne");

        assert_eq!(l.peek_char(), Some('H'));

        assert_eq!(
            l.pos,
            Pos {
                line: 0,
                col: 0,
                filename: l.pos.filename.to_owned(),
            }
        );

        assert_eq!(l.next_char(), Some('H'));
        assert_eq!(
            l.pos,
            Pos {
                line: 0,
                col: 1,
                filename: l.pos.filename.to_owned(),
            }
        );

        assert_eq!(l.peek_char(), Some('\n'));
        assert_eq!(l.next_char(), Some('\n'));
        assert_eq!(
            l.pos,
            Pos {
                line: 1,
                col: 0,
                filename: l.pos.filename.to_owned(),
            }
        );

        l.push_char('a');
        assert_eq!(l.peek_char(), Some('a'));
        assert_eq!(l.next_char(), Some('a'));
        assert_eq!(
            l.pos,
            Pos {
                line: 1,
                col: 1,
                filename: l.pos.filename.to_owned(),
            }
        );

        assert_eq!(l.peek_char(), Some('e'));
        assert_eq!(l.next_char(), Some('e'));
        assert_eq!(
            l.pos,
            Pos {
                line: 1,
                col: 2,
                filename: l.pos.filename.to_owned(),
            }
        );
        assert_eq!(l.peek_char(), None);
        assert_eq!(l.next_char(), None);
        assert_eq!(
            l.pos,
            Pos {
                line: 1,
                col: 2,
                filename: l.pos.filename.to_owned(),
            }
        );
    }

    #[test]
    fn operators_test() {
        let l = Lexer::new();
        assert_eq!(
            l.operators_lead_char,
            HashSet::from([
                '|', '(', '?', '^', '<', '/', '[', '.', '-', ',', '=', '*', '>', '~', '!', '+',
                '%', '&'
            ],)
        );
        assert_eq!(
            l.operators_following_chars,
            HashSet::from(['-', '+', '&', '|', '>', '<', '='])
        );
    }
    #[test]
    fn parse_num_and_string_test() {
        let l = Lexer::new();
        let mut fp = create_temp_filelexer("23543   2123  \"acbsd\"");

        let expected = vec![
            Token {
                tv: TokenTypeAndValue::Number(Num::ULongLongInt(23543)),
                pos: Pos {
                    line: 0,
                    col: 0,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: true,
            },
            Token {
                tv: TokenTypeAndValue::Number(Num::ULongLongInt(2123)),
                pos: Pos {
                    line: 0,
                    col: 8,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: true,
            },
            Token {
                tv: TokenTypeAndValue::Str("acbsd".to_string()),
                pos: Pos {
                    line: 0,
                    col: 15,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
        ];
        assert_eq!(l.parse(&mut fp), &expected);
    }

    #[test]
    fn parse_operator_and_symbol_test() {
        let l = Lexer::new();
        let mut fp = create_temp_filelexer("3+2++(50)");

        let expected = vec![
            Token {
                tv: TokenTypeAndValue::Number(Num::ULongLongInt(3)),
                pos: Pos {
                    line: 0,
                    col: 0,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Operator("+".to_string()),
                pos: Pos {
                    line: 0,
                    col: 1,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Number(Num::ULongLongInt(2)),
                pos: Pos {
                    line: 0,
                    col: 2,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Operator("++".to_string()),
                pos: Pos {
                    line: 0,
                    col: 3,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Operator("(".to_string()),
                pos: Pos {
                    line: 0,
                    col: 5,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Number(Num::ULongLongInt(50)),
                pos: Pos {
                    line: 0,
                    col: 6,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Symbol(')'),
                pos: Pos {
                    line: 0,
                    col: 8,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
        ];
        assert_eq!(l.parse(&mut fp), &expected);
    }

    #[test]
    fn parse_identifier_or_keyword_or_newline_test() {
        env_logger::init();
        let l = Lexer::new();
        let mut fp = create_temp_filelexer("gewa __ignore_typecheck__ week\n");

        let expected = vec![
            Token {
                tv: TokenTypeAndValue::Identifier("gewa".to_string()),
                pos: Pos {
                    line: 0,
                    col: 0,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: true,
            },
            Token {
                tv: TokenTypeAndValue::Keyword("__ignore_typecheck__".to_string()),
                pos: Pos {
                    line: 0,
                    col: 5,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: true,
            },
            Token {
                tv: TokenTypeAndValue::Identifier("week".to_string()),
                pos: Pos {
                    line: 0,
                    col: 26,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
            Token {
                tv: TokenTypeAndValue::Newline,
                pos: Pos {
                    line: 0,
                    col: 30,
                    filename: fp.pos.filename.to_owned(),
                },
                whitespace_tail: false,
            },
        ];
        assert_eq!(l.parse(&mut fp), &expected);
    }

    fn create_temp_filelexer(init_content: &str) -> FileLexer {
        let mut f1 = create_temp_file();
        let f2 = create_temp_file();
        write!(f1, "{init_content}").expect("Failed to write to temp file");
        let l = FileLexer::new(f1.path(), f2.path()).expect("Failed to create lexer");
        l
    }
    fn create_temp_file() -> NamedTempFile {
        let file = NamedTempFile::new().expect("Failed to create temp file");
        assert_eq!(file.path().exists(), true);
        file
    }
}
