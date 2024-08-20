use lazy_static::lazy_static;
use regex::Regex;

use crate::lexer::cursor::Cursor;
use crate::symbol::Register;

pub mod cursor;

/// A 'light' token that only carries basic and easily derivable info
#[derive(Debug)]
pub struct LToken {
    pub kind: LTokenKind,
    pub len: u32,
}

impl LToken {
    pub fn new(kind: LTokenKind, len: u32) -> Self {
        LToken { kind, len }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LiteralKind {
    Hex,
    Dec,
    Str { terminated: bool },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LTokenKind {
    Ident,
    Lit(LiteralKind),
    Comment,
    Direc,
    Reg,
    /// Also includes commas
    Whitespace,
    Unknown,
    Eof,
}

/// Not actually used in parsing, more for debug purposes.
pub fn tokenize(input: &str) -> impl Iterator<Item = LToken> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != LTokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

/// Test if a character is considered to be whitespace.
pub(crate) fn is_whitespace(c: char) -> bool {
    // Commas are essentially whitespace in LC3
    matches!(c, ' ' | '\n' | '\t' | '\r' | ',')
}

pub(crate) fn is_reg_num(c: char) -> bool {
    // Valid only between 0-7
    matches!(c, '0'..='7')
}

/// Test if a character is considered an LC3 identifier character.
pub(crate) fn is_id(c: char) -> bool {
    // Non-prefixed numerical literals are considered identifiers.
    // This is because line numbers can be used as labels.
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> LToken {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return LToken::new(LTokenKind::Eof, 0),
        };
        let token_kind = match first_char {
            ';' => {
                self.take_while(|c| c != '\n');
                LTokenKind::Comment
            }
            c if is_whitespace(c) => {
                self.take_while(is_whitespace);
                LTokenKind::Whitespace
            }
            // Hex literals
            'x' | 'X' => self.hex(),
            '0' => match self.first() {
                'x' | 'X' => self.hex(),
                _ => self.ident(),
            },
            'r' | 'R' => match self.first() {
                c if is_reg_num(c) => {
                    self.take_while(is_reg_num);
                    // Registers are 2 tokens long and followed by whitespace/comma
                    if self.pos_in_token() == 2 && is_whitespace(self.first()) {
                        LTokenKind::Reg
                    } else {
                        self.ident()
                    }
                }
                _ => self.ident(),
            },
            // Identifiers should be checked after everything else that overlaps.
            c if is_id(c) => self.ident(),
            // Decimal literal
            '#' => {
                if self.first() == '-' {
                    self.bump();
                }
                self.take_while(|c| char::is_ascii_digit(&c));
                LTokenKind::Lit(LiteralKind::Dec)
            }
            // Directive
            '.' => {
                let check = self.take_n(3).to_ascii_lowercase();
                self.take_while(is_id);
                // Need to check for .end directive to avoid unnecessary parsing and errors
                match (self.pos_in_token(), check.as_str()) {
                    (3, "end") => LTokenKind::Eof,
                    _ => LTokenKind::Direc,
                }
            }
            // String literal
            // TODO: Allow for escaped characters and the terminated thing
            '"' => {
                self.take_while(|c| c != '"');
                LTokenKind::Lit(LiteralKind::Str { terminated: true })
            }
            _ => LTokenKind::Unknown,
        };
        let res = LToken::new(token_kind, self.pos_in_token());
        self.reset_pos();
        res
    }

    fn ident(&mut self) -> LTokenKind {
        self.take_while(|c| char::is_ascii_hexdigit(&c));
        LTokenKind::Ident
    }

    fn hex(&mut self) -> LTokenKind {
        self.take_while(|c| char::is_ascii_hexdigit(&c));
        LTokenKind::Lit(LiteralKind::Hex)
    }
}
