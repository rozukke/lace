use std::{
    borrow::{Borrow, Cow},
    fmt::Debug,
    iter::{Filter, Peekable},
    ops::RangeBounds,
    vec::IntoIter,
};

use miette::{bail, miette, LabeledSpan, Result, Severity};

use crate::{
    air::{Air, AirStmt},
    lexer::{cursor::Cursor, LiteralKind, Token, TokenKind},
    symbol::{with_symbol_table, DirKind, Label, Span},
};

/// Replaces raw value directives .fill, .blkw, .stringz with equivalent raw bytes
/// Returns a 'final' vector of tokens. This is easier than working with an iterator that can
/// either return a single token or a Vec of tokens.
pub fn preprocess(src: &str) -> Result<Vec<Token>> {
    let mut res: Vec<Token> = Vec::new();
    let mut cur = Cursor::new(src);

    // TODO: Should handle directives not having a label as this is the last time they exist within
    // control flow.
    loop {
        let dir = cur.advance_token()?;
        // TODO: Whitespace... really need to get rid of it earlier
        cur.advance_token()?;
        match dir.kind {
            // Preprocess .fill into a raw byte with the next literal as value
            TokenKind::Dir(DirKind::Fill) => {
                // Must be inside to avoid skipping tokens
                let val = cur.advance_token()?;
                // Maybe fix code duplication here?
                match val.kind {
                    TokenKind::Lit(LiteralKind::Hex(lit)) => {
                        res.push(Token::byte(lit));
                    }
                    TokenKind::Lit(LiteralKind::Dec(lit)) => {
                        res.push(Token::byte(lit as u16));
                    }
                    _ => bail!(
                        severity = Severity::Error,
                        code = "preproc::fill",
                        help = "The .fill directive requires an integer or hex literal value.",
                        labels = vec![LabeledSpan::at(val.span, "incorrect literal")],
                        "Expected an integer or hex literal.",
                    ),
                }
            }
            // Preprocess .blkw into a series of raw null bytes
            TokenKind::Dir(DirKind::Blkw) => {
                let val = cur.advance_token()?;
                match val.kind {
                    TokenKind::Lit(LiteralKind::Hex(lit)) => {
                        // Empty bytes
                        for _ in 0..lit {
                            res.push(Token::nullbyte());
                        }
                    }
                    TokenKind::Lit(LiteralKind::Dec(lit)) => {
                        if lit < 0 {
                            bail!(
                                severity = Severity::Error,
                                code = "preproc::blkw",
                                help = "try a positive literal like #1",
                                labels = vec![LabeledSpan::at(val.span, "negative literal")],
                                "Integer values for .blkw cannot be negative.",
                            )
                        }
                        // Empty bytes
                        for _ in 0..lit {
                            res.push(Token::nullbyte());
                        }
                    }
                    _ => bail!(
                        severity = Severity::Error,
                        code = "preproc::blkw",
                        help = ".blkw requires a non-negative integer or hex literal value.",
                        labels = vec![LabeledSpan::at(val.span, "not a literal")],
                        "Expected an non-negative integer or hex literal.",
                    ),
                }
            }
            // Preprocess string literal into a sequence of bytes corresponding to a literal with a
            // null terminator.
            TokenKind::Dir(DirKind::Stringz) => {
                let val = cur.advance_token()?;
                match val.kind {
                    TokenKind::Lit(LiteralKind::Str) => {
                        let str_raw = cur.get_range(val.span.into());
                        // Get rid of start and end \"
                        for c in unescape(&str_raw[1..str_raw.len() - 1]).chars() {
                            res.push(Token::byte(c as u16));
                        }
                        // Terminating null byte
                        res.push(Token::nullbyte());
                    }
                    _ => bail!(
                        severity = Severity::Error,
                        code = "preproc::stringz",
                        help = ".stringz requires a valid string literal like \"hello\n\"",
                        labels = vec![LabeledSpan::at(val.span, "not a string literal")],
                        "Expected a valid string literal.",
                    ),
                }
            }
            // Can eliminate these from here
            TokenKind::Comment | TokenKind::Whitespace => continue,
            TokenKind::Eof => break,
            _ => res.push(dir),
        }
    }
    Ok(res)
}

// TODO: Kind of ugly, see if improvable
fn unescape(s: &str) -> Cow<str> {
    if s.find('\\').is_none() {
        return Cow::Borrowed(s);
    }
    let mut result = String::new();
    let mut chars = s.chars();
    let mut needs_allocation = false;

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => {
                    // Trailing backslash; include it as is
                    result.push('\\');
                }
            }
            needs_allocation = true;
        } else {
            result.push(c);
        }
    }
    Cow::Owned(result)
}

/// Transforms token stream into AIR
pub struct AsmParser<'a> {
    /// Reference to the source file
    src: &'a str,
    /// Peekable iterator over preprocessed tokens
    toks: Peekable<IntoIter<Token>>,
    /// Assembly intermediate representation
    air: Air,
    /// Tracker for current line
    line: u16,
}

impl<'a> AsmParser<'a> {
    /// Takes in preprocessed tokens or will otherwise go into unreachable code. Input should
    /// contain no whitespace or comments.
    pub fn new(src: &'a str, toks: Vec<Token>) -> Self {
        AsmParser {
            src,
            toks: toks.into_iter().peekable(),
            air: Air::new(),
            line: 1,
        }
    }

    fn get_span(&self, span: Span) -> &str {
        &self.src[span.offs()..span.end()]
    }

    /// Create AIR out of token stream
    pub fn parse(mut self) -> Result<Air> {
        loop {
            self.line += 1;
            // TODO: Might be a better pattern for this
            if self.toks.peek().is_none() {
                break;
            }
            // Can/has to take label: bytes, instr, trap
            if let Some(label) = self.get_label() {
                // Add prefix label to symbol table
                let symbol = match Label::insert(self.get_span(label.span), self.line) {
                    Ok(sym) => sym,
                    Err(_) => bail!(
                        severity = Severity::Error,
                        code = "parse::duplicate_label",
                        help = "labels are only allowed once per file",
                        labels = vec![LabeledSpan::at(label.span, "duplicate label")],
                        "Duplicate prefix label"
                    ),
                };

                if let Some(tok) = self.toks.peek() {
                    match tok.kind {
                        // Invalid after label
                        TokenKind::Label
                        | TokenKind::Lit(_)
                        | TokenKind::Reg(_)
                        // This is actually only .orig at this stage
                        | TokenKind::Dir(_) => bail!(
                            severity = Severity::Error,
                            code = "parse::unexpected_token",
                            help = "labels should be followed by an instruction, trap, or directive.",
                            labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                            "Unexpected token type: {}", tok.kind
                        ),
                        TokenKind::Instr(_) => self.parse_instr(Some(symbol))?,
                        TokenKind::Trap(_) => self.parse_trap(Some(symbol))?,
                        TokenKind::Byte(_) => self.parse_byte(symbol)?,
                        // Does not exist in preprocessed token stream
                        TokenKind::Whitespace
                        | TokenKind::Comment
                        | TokenKind::Eof => unreachable!(),
                    }
                } else {
                    bail!(
                        severity = Severity::Error,
                        code = "parse::unexpected_eof",
                        help = "labels should be followed by an instruction, trap, or directive.",
                        labels = vec![LabeledSpan::at_offset(label.span.end(), "unexpected eof")],
                        "Unexpected end of file"
                    )
                }
            }
            // May drop label/has to drop label: .orig, instr, trap
            else {
                // TODO: crappy hack but cbs thinking about this
                let mut peek = self.toks.clone();
                if let Some(tok) = peek.next() {
                    match tok.kind {
                        TokenKind::Instr(_) => self.parse_instr(None)?,
                        TokenKind::Trap(_) => self.parse_trap(None)?,
                        TokenKind::Dir(_) => {
                            let val = match peek.next() {
                                Some(val) => val,
                                None =>
                                    bail!(
                                        severity = Severity::Error,
                                        code = "parse::unexpected_eof",
                                        help = "labels should be followed by an instruction, trap, or directive.",
                                        labels = vec![LabeledSpan::at_offset(tok.span.end(), "unexpected eof")],
                                        "Unexpected end of file"
                                    )
                            };
                            let val = match val.kind {
                                TokenKind::Lit(_) => todo!(),
                                _ => bail!(
                                    severity = Severity::Error,
                                    code = "parse::expected_lit",
                                    help = ".orig should be followed by a non-negative integer or hex literal.",
                                    labels = vec![LabeledSpan::at(val.span, "invalid literal")],
                                    "Unexpected token type: {}", val.kind
                                )
                            };
                            self.air.set_orig(val)?;
                        }
                        // TODO: Better errors for directives without a label
                        TokenKind::Lit(_) | TokenKind::Reg(_) | TokenKind::Byte(_) => {
                            bail!(
                                severity = Severity::Error,
                                code = "parse::unexpected_token",
                                help =
                                    "lines should start with an instruction, trap, or directive.",
                                labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                                "Unexpected token type: {}",
                                tok.kind
                            )
                        }
                        // Not present at this stage of assembly
                        TokenKind::Whitespace
                        | TokenKind::Comment
                        | TokenKind::Eof
                        | TokenKind::Label => {
                            unreachable!()
                        }
                    }
                } else {
                    break;
                }
            }
        }
        // Consume self to return AIR
        Ok(self.air)
    }

    /// Return label or leave iter untouched and return None
    fn get_label(&mut self) -> Option<Token> {
        if let Some(tok) = self.toks.peek() {
            return match tok.kind {
                TokenKind::Label => {
                    // Guaranteed to be Some
                    Some(self.toks.next().unwrap())
                }
                _ => None,
            };
        }
        None
    }

    /// Process several tokens to form valid instruction AIR
    fn parse_instr(&mut self, label: Option<Label>) -> Result<()> {
        // Presence checked before call
        let instr = self.toks.next().unwrap();
        let instr = match instr.kind {
            TokenKind::Instr(instr) => instr,
            _ => unreachable!(),
        };

        use crate::symbol::InstrKind;
        let stmt = match instr {
            InstrKind::Add => todo!(),
            InstrKind::And => todo!(),
            InstrKind::Br(flag) => todo!(),
            InstrKind::Jmp => {
                let target =
                    match self.expect_where(|tok| matches!(tok, TokenKind::Reg(_)), "register") {
                        Ok(TokenKind::Reg(val)) => val,
                        Err(err) => return Err(err),
                        _ => unreachable!(),
                    };
                AirStmt::Jump {
                    label,
                    base_r: target,
                }
            }
            InstrKind::Jsr => todo!(),
            InstrKind::Jsrr => todo!(),
            InstrKind::Ld => todo!(),
            InstrKind::Ldi => todo!(),
            InstrKind::Ldr => todo!(),
            InstrKind::Lea => todo!(),
            InstrKind::Not => todo!(),
            InstrKind::Ret => todo!(),
            InstrKind::Rti => todo!(),
            InstrKind::St => todo!(),
            InstrKind::Sti => todo!(),
        };

        self.air.add_stmt(stmt);
        Ok(())
    }

    fn parse_trap(&mut self, label: Option<Label>) -> Result<()> {
        todo!()
    }

    fn parse_byte(&mut self, label: Label) -> Result<()> {
        todo!()
    }

    fn expect(&self, kind: TokenKind) -> Result<()> {
        todo!()
    }

    fn expect_where(
        &mut self,
        mut check: impl FnMut(&TokenKind) -> bool,
        expected: &str,
    ) -> Result<TokenKind> {
        match self.toks.next() {
            Some(tok) if check(&tok.kind) => Ok(tok.kind),
            None => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_eof",
                help = "check the number of operands required for this instruction.",
                // labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                "Unexpected end of file",
            ),
            Some(unexpected) => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_token",
                help = "check the type of operands allowed for this instruction.",
                labels = vec![LabeledSpan::at(unexpected.span, "unexpected token")],
                "Expected token of type {}, found {}",
                expected,
                unexpected.kind
            ),
        }
    }
}

mod tests {
    use std::collections::VecDeque;

    use crate::lexer::{Token, TokenKind};

    use super::preprocess;

    // .FILL TEST
    #[test]
    fn preproc_fill() {
        let res = preprocess(".fill x3000").unwrap();
        assert!(res[0].kind == TokenKind::Byte(0x3000))
    }

    #[test]
    fn preproc_fill_neg() {
        let res = preprocess(".fill #-35").unwrap();
        assert!(res[0].kind == TokenKind::Byte(-35i16 as u16))
    }

    #[test]
    fn preproc_fill_dec() {
        let res = preprocess(".fill #3500").unwrap();
        assert!(res[0].kind == TokenKind::Byte(3500))
    }

    #[test]
    fn preproc_fill_invalid() {
        assert!(preprocess(".fill add").is_err())
    }

    // .BLKW TEST
    #[test]
    fn preproc_blkw_basic() {
        let res = preprocess(".blkw x2")
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokenKind>>();
        assert!(res == vec![TokenKind::Byte(0), TokenKind::Byte(0)]);

        let res = preprocess(".blkw #3")
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokenKind>>();
        assert!(res == vec![TokenKind::Byte(0), TokenKind::Byte(0), TokenKind::Byte(0)])
    }

    #[test]
    fn preproc_blkw_neg() {
        assert!(preprocess(".blkw #-3").is_err())
    }

    #[test]
    fn preproc_blkw_invalid() {
        assert!(preprocess(".blkw add").is_err())
    }

    // .STRINGZ TEST
    #[test]
    fn preproc_stringz_escaped() {
        // .blkw "\"hello\"\n" => "hello"
        let res = preprocess(r#".stringz "\"hello\n\"""#).unwrap();
        let expected = "\"hello\n\"\0"
            .chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res == expected)
    }

    #[test]
    fn preproc_stringz_standard() {
        // .blkw "hello" => hello
        let res = preprocess(r#".stringz "hello""#).unwrap();
        let expected = "hello\0"
            .chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res == expected)
    }

    #[test]
    fn preproc_stringz_invalid() {
        assert!(preprocess(r#".stringz error"#).is_err())
    }
}
