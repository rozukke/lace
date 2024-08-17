use std::{borrow::BorrowMut, error::Error};

use miette::{miette, Result};

use crate::{
    lexer::{cursor::Cursor, tokenize, LToken, LTokenKind, LiteralKind},
    symbol::{
        with_symbol_table, ByteOffs, DirKind, InstrKind, Register, Span, Symbol, TrapKind,
        SYMBOL_TABLE,
    },
};

/// Token with full span info and proper types
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

/// Used to parse symbols and process exact instructions
pub struct StrParser<'a> {
    src: &'a str,
    cur: Cursor<'a>,
    pos: usize,
    line_num: usize,
}

impl<'a> StrParser<'a> {
    pub fn new(src: &'a str) -> Self {
        StrParser {
            src,
            cur: Cursor::new(src),
            pos: 0,
            line_num: 1,
        }
    }

    fn get_next(&self, n: usize) -> &str {
        &self.src[self.pos..=(self.pos + n)]
    }

    pub fn proc_tokens(&mut self) -> Vec<Token> {
        // Iterate through, +1 to symbol count per inst
        // +len(str) for every string literal
        // +number of lines for BLKW (need to process cringe inconsistent literals)
        // Also need to do matching to process register and instruction tokens into the correct contents
        let mut toks_final: Vec<Token> = Vec::new();
        let mut line_num = 1;
        loop {
            let tok = self.cur.advance_token();
            if let Some(tok_final) = match tok.kind {
                // Add identifier to symbol table at with correct line number
                LTokenKind::Ident => {
                    // Process possibility of it being a trap
                    todo!();
                    // Add to symbol table as identifier
                    let idx = with_symbol_table(|sym| {
                        let tok_text = self.get_next(tok.len as usize);
                        sym.get_index_of(tok_text)
                            .unwrap_or(sym.insert_full(String::from(tok_text), line_num).0)
                    });
                    Some(Token {
                        kind: TokenKind::Label(Symbol::from(idx)),
                        span: Span::new(ByteOffs(self.pos), tok.len as usize),
                    })
                }
                // Create literal of correct value
                LTokenKind::Lit(_) => todo!(),
                // Match on directive, check next value for number of lines skipped
                LTokenKind::Direc => todo!(),
                // TODO: Add registers to lexer
                LTokenKind::Reg => todo!(),
                LTokenKind::Whitespace | LTokenKind::Comment => None,
                // TODO: Should return list of errors eventually
                LTokenKind::Unknown => todo!(),
                LTokenKind::Eof => break,
            } {
                toks_final.push(tok_final);
            }
        }
        toks_final
    }
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
        let tok: Vec<Token> = StrParser::new(src).proc_tokens();
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
