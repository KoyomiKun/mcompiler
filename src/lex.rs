#![allow(dead_code)]
/// support utf-8
use std::{
    collections::HashSet,
    fmt::Write,
    fs::{File, OpenOptions},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use anyhow::Result;
use log::error;

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

const OPERATORS: [char; 16] = [
    '+', '-', '*', '>', '<', '^', '%', '!', '=', '~', '|', '&', '(', '[', ',', '.',
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
    fn get_last_token(&mut self) -> Option<&mut Token>;

    fn error(&self, err: anyhow::Error) -> String;
    fn copy_position(&self) -> Pos;
}

struct Lexer<'a> {
    keywords: HashSet<&'a str>,
    /// operators can be combined such as "<<" or ">>"
    // technically brackets are operator, but when we found a symbol ')' we known operator '('
    // done. that's why we put ')' in symbols
    operators: HashSet<char>,
    symbols: HashSet<char>,
}

struct FileLexer {
    input_reader: BufReader<File>,
    // current_line contains '\n'
    current_line: String,
    output_file: File,

    pos: Pos,
    current_expression_count: usize,
    parentheeses_buffer: String,

    tokens: Vec<Token>,
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

    fn error(&self, err: anyhow::Error) -> String {
        format!(
            "on line {}, col {} in file {}, {}",
            self.pos.line,
            self.pos.col,
            self.pos.filename.to_string_lossy().to_string(),
            err,
        )
    }

    fn push_token(&mut self, t: Token) {
        self.tokens.push(t)
    }

    fn get_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    fn get_last_token(&mut self) -> Option<&mut Token> {
        self.tokens.last_mut()
    }
}

impl<'a> Lexer<'a> {
    pub fn new() -> Self {
        Self {
            keywords: HashSet::from(KEYWORDS),
            operators: HashSet::from(OPERATORS),
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
            if Self::is_digit(c) {
                match self.make_num_token(parser) {
                    Err(e) => {
                        error!("make num token failed: {}", parser.error(e));
                        return None;
                    }
                    Ok(t) => {
                        return Some(t);
                    }
                }
            }

            if c.is_whitespace() {}
        }
        None
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

    // parse whitespaces
    fn make_whitespace_token<T: LexParser>(&self, parser: &mut T) -> Result<Token> {
        let mut buf = String::new();
        let p = parser.copy_position();
        lex_getc_if!(parser, Self::is_whitespace, buf);

        Ok(Token {
            tv: TokenTypeAndValue::(Num::ULongLongInt(buf.parse()?)),
            pos: p,
            whitespace_tail: false,
        })
    }

    fn is_digit(c: char) -> bool {
        ('0'..='9').contains(&c)
    }

    fn is_whitespace(c: char) -> bool {
        c.is_whitespace()
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
            parentheeses_buffer: String::new(),

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
    fn parse_num_test() {
        let l = Lexer::new();
        let mut fp = create_temp_filelexer("23543 2123");

        let expected = vec![Token {
            tv: TokenTypeAndValue::Number(Num::ULongLongInt(23543)),
            pos: Pos {
                line: 0,
                col: 0,
                filename: fp.pos.filename.to_owned(),
            },
            whitespace_tail: false,
        }];

        for (act, exp) in l.parse(&mut fp).into_iter().zip(expected.into_iter()) {
            assert_eq!(act, &exp)
        }
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
