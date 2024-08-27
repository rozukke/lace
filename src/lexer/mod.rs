use std::str::FromStr;

use miette::{Result, bail, miette, LabeledSpan, Severity};

use crate::lexer::cursor::Cursor;
use crate::symbol::{DirKind, InstrKind, Register, Span, SrcOffset, TrapKind};

pub mod cursor;

/// A 'light' token that carries basic info and span
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LiteralKind {
    Hex(u16),
    Dec(i16),
    Str,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Label,
    Instr(InstrKind),
    Trap(TrapKind),
    Lit(LiteralKind),
    Dir(DirKind),
    Reg(Register),
    /// Also includes commas
    Whitespace,
    Unknown,
    Comment,
    Eof,
}

/// Not actually used in parsing, more for debug purposes.
pub fn tokenize(input: &str) -> impl Iterator<Item = Result<Token>> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        loop {
            let token = cursor.advance_token();
            if let Ok(inner) = &token {
                if inner.kind == TokenKind::Whitespace {
                    continue;
                }
                if inner.kind == TokenKind::Eof {
                    return None;
                }
            }
            return Some(token);
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
    pub fn advance_token(&mut self) -> Result<Token> {
        let start_pos = self.abs_pos();
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Ok(Token::new(TokenKind::Eof, Span::dummy())),
        };
        let token_kind = match first_char {
            ';' => {
                self.take_while(|c| c != '\n');
                TokenKind::Comment
            }
            c if is_whitespace(c) => {
                self.take_while(is_whitespace);
                TokenKind::Whitespace
            }
            // Hex literals
            'x' | 'X' => self.hex()?,
            '0' => match self.first() {
                'x' | 'X' => {
                    self.bump();
                    self.hex()?
                    },
                _ => self.ident(),
            },
            'r' | 'R' => match self.first() {
                c if is_reg_num(c) => {
                    self.take_while(is_reg_num);
                    // Registers are 2 tokens long and followed by whitespace/comma
                    if self.pos_in_token() == 2 && is_whitespace(self.first()) {
                        // Unwrap is safe as c is always valid.
                        TokenKind::Reg(Register::from_str(&c.to_string()).unwrap())
                    } else {
                        self.ident()
                    }
                }
                _ => self.ident(),
            },
            // Identifiers should be checked after everything else that overlaps.
            c if is_id(c) => self.ident(),
            // Decimal literal
            '#' => self.dec()?,
            // Directive
            // '.' => {
            //     let check = self.take_n(3).to_ascii_lowercase();
            //     self.take_while(is_id);
            //     // Need to check for .end directive to avoid unnecessary parsing and errors
            //     match (self.pos_in_token(), check.as_str()) {
            //         (3, "end") => TokenKind::Eof,
            //         _ => TokenKind::Dir,
            //     }
            // }
            // String literal
            '"' => self.string_literal()?,
            _ => {
                self.take_while(|c| !is_whitespace(c));
                TokenKind::Unknown
            },
        };
        let res = Token::new(token_kind, Span::new(SrcOffset(start_pos), self.pos_in_token()));
        self.reset_pos();
        Ok(res)
    }

    fn ident(&mut self) -> TokenKind {
        self.take_while(is_id);
        TokenKind::Label
    }

    fn hex(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        self.take_while(|c| !is_whitespace(c));
        let str_val = self.get_range(start..self.abs_pos());
        let value = match u16::from_str_radix(str_val, 16) {
            Ok(value) => value,
            Err(e) => {
                return Err(miette!(
                    severity = Severity::Error,
                    code = "parse::hex_lit",
                    help = "only use characters 0-9 and a-F.",
                    labels = vec![LabeledSpan::at(start - prefix..self.abs_pos(), "incorrect literal")],
                    "Encountered an invalid hex literal: {e}",
                ))
            }
        };

        Ok(TokenKind::Lit(LiteralKind::Hex(value)))
    }

    fn dec(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        // Check for negative sign
        let is_negative = if self.first() == '-' {
            self.bump(); // Skip the negative sign
            true
        } else {
            false
        };
        // Take the numeric part
        self.take_while(|c| char::is_ascii_digit(&c));
        let str_val = self.get_range(start..self.abs_pos());

        // Parse the string as an i16 to handle negative values
        let value = match i16::from_str_radix(&str_val, 10) {
            Ok(value) => value,
            Err(e) => {
                bail!(
                    severity = Severity::Error,
                    code = "parse::dec_lit",
                    help = "LC3 supports 16 bits of space, from -32,768 to 32,767.",
                    labels = vec![LabeledSpan::at(start - prefix..self.abs_pos(), "incorrect literal")],
                    "Encountered an invalid decimal literal: {e}",
                )
            }
        };

        Ok(TokenKind::Lit(LiteralKind::Dec(value)))
    }

    fn string_literal(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos() - 1;
        let mut terminated = false;
        while let Some(c) = self.bump() {
            if c == '\n' {break};
            if c == '"' {
                terminated = true;
                break;
            }
            // Skip escaped
            if c == '\\' {
                self.bump();
            }
        }
        if !terminated {
            bail!(
                severity = Severity::Error,
                code = "parse::str_lit",
                help = "hint: make sure to close string literals with a \" character.",
                labels = vec![LabeledSpan::at(start..self.abs_pos(), "incorrect literal")],
                "Encountered an unterminated string literal.",

        )
        }
        Ok(TokenKind::Lit(LiteralKind::Str))
    }
}
