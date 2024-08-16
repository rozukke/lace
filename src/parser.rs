use std::error::Error;

use miette::{miette, Result};

use crate::{
    lexer::{cursor::Cursor, tokenize, LToken, LTokenKind, LiteralKind},
    symbol::{DirKind, InstrKind, Register, Span, Symbol, TrapKind},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    /// `r0-r7 | R0-R7`
    Reg(Register),
    /// `LOOP_START`, `123`, `coolname`
    Label(Symbol),
    /// `.orig`, `.Stringz`, `.BLKW`
    Dir(DirKind),
    /// `PUTS`, `Trap`, `putc`
    Trap(TrapKind),
    /// `"hi\n"`, `0x3AB5F`, `#-1`
    Lit(LiteralKind),
    /// `add`, `JMP`, `Ret`
    Inst(InstrKind),
}

pub fn proc_tokens(src: &str) -> Vec<Token> {
    // Get reference to global symbol table
    // Iterate through, +1 to symbol count per inst
    // +len(str) for every string literal
    // +number of lines for BLKW (need to process cringe inconsistent literals)
    // Also need to do matching to process register and instruction tokens into the correct contents
    let toks: Vec<LToken> = tokenize(src).collect();
    todo!()
}

/// Transforms token stream into 'AST'
pub struct AsmParser<'a> {
    /// Reference to the source file
    src: &'a str,
    /// List of processed tokens
    tok: Vec<Token>,
    /// Used to parse tokens
    cur: Cursor<'a>,
}

impl<'a> From<&'a str> for AsmParser<'a> {
    fn from(src: &'a str) -> Self {
        let tok: Vec<Token> = proc_tokens(src);
        AsmParser {
            src,
            tok,
            cur: Cursor::new(src),
        }
    }
}

impl<'a> AsmParser<'a> {
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
