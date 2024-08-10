use crate::lexer::{cursor::Cursor, TokenKind};

/// Transforms token stream into 'AST'
pub struct Parser<'source> {
    /// Reference to the source file
    src: &'source str,
    /// Used to parse tokens
    cur: Cursor<'source>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        Parser {
            src: value,
            cur: Cursor::new(value),
        }
    }
}

impl<'source> Parser<'source> {
    pub fn parse(&self) {
        // First, check that there is an .orig directive with an appropriate value.
        todo!()
    }

    pub fn expect(kind: TokenKind) {
        todo!()
    }

    pub fn parse_direc(&self) {
        todo!()
    }

    pub fn parse_op(&self) {
        todo!()
    }
}
