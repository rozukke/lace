use std::error::Error;

use miette::{miette, Result};

use crate::lexer::{cursor::Cursor, Token, TokenKind};

/// Transforms token stream into 'AST'
pub struct AsmParser<'source> {
    /// Reference to the source file
    src: &'source str,
    /// Used to parse tokens
    cur: Cursor<'source>,
}

impl<'a> From<&'a str> for AsmParser<'a> {
    fn from(value: &'a str) -> Self {
        AsmParser {
            src: value,
            cur: Cursor::new(value),
        }
    }
}

impl<'source> AsmParser<'source> {
    pub fn parse(&mut self) -> Result<()> {
        // First, check that there is an .orig directive with an appropriate value.
        let orig = self.expect(TokenKind::Direc)?;
        let addr = self.expect(TokenKind::Lit(crate::lexer::LiteralKind::Hex));

        Ok(())
    }

    pub fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let tok = self.cur.advance_token();
        if tok.kind == kind {
            return Ok(tok);
        }
        Err(miette!(
            "ParseError: expected token of type {:?}, found {:?}",
            kind,
            tok
        ))
    }

    pub fn parse_direc(&self) {
        todo!()
    }

    pub fn parse_op(&self) {
        todo!()
    }
}
