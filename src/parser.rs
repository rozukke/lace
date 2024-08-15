use std::error::Error;

use miette::{miette, Result};

use crate::lexer::{cursor::Cursor, LToken, LTokenKind};

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
        // Should emit error with a label to the first line stating "Expected memory init"
        // Should be in a function that is also used to init the memory - the question is
        // whether it should remain as a full directive or as a value that gets emitted afterwards.
        let orig = self.expect(LTokenKind::Direc)?;
        // Need ability to expect an enum without specifying a subcase (maybe ()?)
        let addr = self.expect(LTokenKind::Lit(crate::lexer::LiteralKind::Hex));

        // Following this, the structure is always:
        // [label]
        // ->   <inst> [args]
        // OR
        // <label>
        // ->   <direc> [args]
        // OR
        // [label]
        // ->*   <direc> <args>
        // OR
        // <trap> [arg]
        // or: (sometimes opt label) num directives (opt argument)
        // so should generally build to this structure. This means, however, that the complexity
        // is not suuper high as there are really only two medium complexity subcases to parse.
        //
        // TODO: Split into LexToken and Token, to simplify the lexer and have a postprocessing
        // step that can then put it into a Token format that is then easily transformed into
        // the 'AST'.
        //
        // In order to do this, there needs to be peeking functionality on the token stream so
        // that it can e.g. see if there is a label present at the start of a line.

        Ok(())
    }

    pub fn expect(&mut self, kind: LTokenKind) -> Result<LToken> {
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
