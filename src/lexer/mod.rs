use lazy_static::lazy_static;
use regex::Regex;

use crate::lexer::cursor::Cursor;
use crate::span::{Idx, Span};
use crate::symbol::Register;

mod cursor;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LiteralKind {
    Hex,
    Dec,
    Str,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Ident,
    Lit(LiteralKind),
    Comment,
    Direc,
    Reg,
    /// Commas and whitespace
    Junk,
    Unknown,
    Eof,
}

/// Not actually used in parsing, more for debug purposes.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

lazy_static! {
    // Order is important since some patterns are subpatterns of others.
    // Do NOT rearrange without a good hard think.
    static ref PATTERNS: Vec<(TokenKind, Regex)> = vec![
        (TokenKind::Junk, Regex::new(r"^[,\s]+").unwrap()),
        (
            TokenKind::Lit(LiteralKind::Hex),
            Regex::new(r"^(0x|x)[0-9a-fA-F]+\b").unwrap(),
        ),
        (
            TokenKind::Lit(LiteralKind::Dec),
            Regex::new(r"^#[0-9]+\b").unwrap(),
        ),
        (TokenKind::Reg, Regex::new(r"^[rR][0-8]\b").unwrap()),
        // Includes instructions, branches, and labels.
        (TokenKind::Ident, Regex::new(r"^[a-zA-Z_]\w*\b").unwrap()),
        (TokenKind::Comment, Regex::new(r"^;[^\n]*").unwrap()),
        (TokenKind::Direc, Regex::new(r"^\.[a-zA-Z_]*\b").unwrap()),
        (TokenKind::Lit(LiteralKind::Str), Regex::new(r#"^"([^"\\]|\\.)*""#).unwrap())
    ];
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Token {
        if self.is_eof() {
            return Token {
                kind: TokenKind::Eof,
                span: Span::default(),
            };
        }

        for (kind, re) in PATTERNS.iter() {
            if let Some(tok) = re.find(self.at_curr_pt()) {
                let token = Token {
                    kind: *kind,
                    span: Span::new(Idx(self.curr_pt() as u32), tok.len() as u16),
                };
                self.advance(tok.len());
                return token;
            }
        }

        self.bump();
        Token {
            kind: TokenKind::Unknown,
            span: Span::new(Idx((self.curr_pt() - 1) as u32), 1u16),
        }
    }
}
