use std::{borrow::Cow, fmt::Display, iter::Peekable, vec::IntoIter};

use miette::{bail, LabeledSpan, Result, Severity};

use crate::{
    air::{Air, AirStmt, ImmediateOrReg, RawWord},
    lexer::{cursor::Cursor, LiteralKind, Token, TokenKind},
    symbol::{DirKind, InstrKind, Label, Register, Span, TrapKind},
};

/// Replaces raw value directives .fill, .blkw, .stringz with equivalent raw bytes
/// Returns a 'final' vector of tokens. This is easier than working with an iterator that can
/// either return a single token or a Vec of tokens.
pub fn preprocess(src: &str) -> Result<Vec<Token>> {
    let mut res: Vec<Token> = Vec::new();
    let mut cur = Cursor::new(src);
    // For keeping track of label presence
    let mut last_token = cur.clone().advance_token()?;

    loop {
        let dir = cur.advance_real()?;
        match dir.kind {
            // Preprocess .fill into a raw byte with the next literal as value
            TokenKind::Dir(DirKind::Fill) => {
                // Must be inside to avoid skipping tokens
                let val = cur.advance_real()?;
                // Check label presence
                if last_token.kind != TokenKind::Label {
                    bail!(
                        severity = Severity::Error,
                        code = "preproc::label",
                        help = ".fill requires a label for sanitation purposes.",
                        labels = vec![LabeledSpan::at(val.span, "unlabeled directive")],
                        "Expected directive to be preceded by a label."
                    )
                }
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
                let val = cur.advance_real()?;
                // Check label presence
                if last_token.kind != TokenKind::Label {
                    bail!(
                        severity = Severity::Error,
                        code = "preproc::label",
                        help = ".blkw requires a label for sanitation purposes.",
                        labels = vec![LabeledSpan::at(val.span, "unlabeled directive")],
                        "Expected directive to be preceded by a label."
                    )
                }
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
                let val = cur.advance_real()?;
                // Check label presence
                if last_token.kind != TokenKind::Label {
                    bail!(
                        severity = Severity::Error,
                        code = "preproc::label",
                        help = ".stringz requires a label for sanitation purposes.",
                        labels = vec![LabeledSpan::at(val.span, "unlabeled directive")],
                        "Expected directive to be preceded by a label."
                    )
                }
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
            TokenKind::Eof | TokenKind::Dir(DirKind::End) => break,
            _ => res.push(dir),
        }
        last_token = dir;
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
    pub fn new(src: &'a str) -> Result<Self> {
        let toks = preprocess(src)?;
        Ok(AsmParser {
            src,
            toks: toks.into_iter().peekable(),
            air: Air::new(),
            line: 1,
        })
    }

    fn get_span(&self, span: Span) -> &str {
        &self.src[span.offs()..span.end()]
    }

    /// Create AIR out of token stream
    pub fn parse(mut self) -> Result<Air> {
        loop {
            let mut labeled_line = false;
            // Add prefix label to symbol table if exists
            if let Some(label) = self.optional_label() {
                labeled_line = true;
                match Label::insert(self.get_span(label.span), self.line) {
                    Ok(_) => (),
                    Err(_) => bail!(
                        severity = Severity::Error,
                        code = "parse::duplicate_label",
                        help = "prefix labels are only allowed once per file",
                        labels = vec![LabeledSpan::at(label.span, "duplicate label")],
                        "Duplicate prefix label"
                    ),
                }
            }

            // Parse line
            if let Some(tok) = self.toks.next() {
                let stmt = match tok.kind {
                    // Lines should not start with these tokens
                    TokenKind::Label | TokenKind::Lit(_) | TokenKind::Reg(_) => {
                        bail!(
                            severity = Severity::Error,
                            code = "parse::unexpected_token",
                            help = "lines should start with an instruction, trap, or directive.",
                            labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                            "Unexpected token of type {}",
                            tok.kind
                        )
                    }
                    TokenKind::Dir(dir) => {
                        // Process .orig as it should be only remaining directive
                        assert!(dir == DirKind::Orig);
                        let orig = self.expect_lit(Bits::Unsigned(16))?;
                        self.air.set_orig(orig)?;
                        continue;
                    }
                    TokenKind::Instr(instr_kind) => self.parse_instr(instr_kind)?,
                    TokenKind::Trap(trap_kind) => self.parse_trap(trap_kind)?,
                    TokenKind::Byte(val) => self.parse_byte(val),
                    // Does not exist in preprocessed token stream
                    TokenKind::Whitespace | TokenKind::Comment | TokenKind::Eof => unreachable!(),
                };
                self.air.add_stmt(stmt);
            } else {
                if !labeled_line {
                    break;
                }
                bail!(
                    severity = Severity::Error,
                    code = "parse::unexpected_eof",
                    help = "Check the number of arguments for the last instruction.",
                    "Unexpected end of file while parsing"
                )
            }

            self.line += 1;
        }
        // Consume self to return AIR
        Ok(self.air)
    }

    /// Return label or leave iter untouched and return None
    fn optional_label(&mut self) -> Option<Token> {
        match self.toks.peek() {
            Some(tok) if tok.kind == TokenKind::Label => Some(self.toks.next().unwrap()),
            _ => None,
        }
    }

    /// Process several tokens to form valid AIR statement
    fn parse_instr(&mut self, kind: InstrKind) -> Result<AirStmt> {
        use crate::symbol::InstrKind;
        match kind {
            InstrKind::Add => {
                let dest = self.expect_reg()?;
                let src_reg = self.expect_reg()?;
                let src_reg_imm = self.expect_lit_or_reg()?;
                Ok(AirStmt::Add {
                    dest,
                    src_reg,
                    src_reg_imm,
                })
            }
            InstrKind::And => {
                let dest = self.expect_reg()?;
                let src_reg = self.expect_reg()?;
                let src_reg_imm = self.expect_lit_or_reg()?;
                Ok(AirStmt::And {
                    dest,
                    src_reg,
                    src_reg_imm,
                })
            }
            InstrKind::Br(flag) => {
                let label_tok = self.expect(TokenKind::Label)?;
                let dest_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::Branch { flag, dest_label })
            }
            InstrKind::Jmp => {
                let src_reg = self.expect_reg()?;
                Ok(AirStmt::Jump { src_reg })
            }
            InstrKind::Jsr => {
                let label_tok = self.expect(TokenKind::Label)?;
                let dest_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::JumbSub { dest_label })
            }
            InstrKind::Jsrr => {
                let src_reg = self.expect_reg()?;
                Ok(AirStmt::JumpSubReg { src_reg })
            }
            InstrKind::Ld => {
                let dest = self.expect_reg()?;
                let label_tok = self.expect(TokenKind::Label)?;
                let src_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::Load { dest, src_label })
            }
            InstrKind::Ldi => {
                let dest = self.expect_reg()?;
                let label_tok = self.expect(TokenKind::Label)?;
                let src_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::LoadInd { dest, src_label })
            }
            InstrKind::Ldr => {
                let dest = self.expect_reg()?;
                let src_reg = self.expect_reg()?;
                let offset = self.expect_lit(Bits::Signed(6))? as u8;
                Ok(AirStmt::LoadOffs {
                    dest,
                    src_reg,
                    offset,
                })
            }
            InstrKind::Lea => {
                let dest = self.expect_reg()?;
                let label_tok = self.expect(TokenKind::Label)?;
                let src_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::LoadEAddr { dest, src_label })
            }
            InstrKind::Not => {
                let dest = self.expect_reg()?;
                let src_reg = self.expect_reg()?;
                Ok(AirStmt::Not { dest, src_reg })
            }
            InstrKind::Ret => Ok(AirStmt::Return),
            InstrKind::Rti => Ok(AirStmt::Interrupt),
            InstrKind::St => {
                let src_reg = self.expect_reg()?;
                let label_tok = self.expect(TokenKind::Label)?;
                let dest_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::Store {
                    src_reg,
                    dest_label,
                })
            }
            InstrKind::Sti => {
                let src_reg = self.expect_reg()?;
                let label_tok = self.expect(TokenKind::Label)?;
                let dest_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::StoreInd {
                    src_reg,
                    dest_label,
                })
            }
        }
    }

    fn parse_trap(&mut self, kind: TrapKind) -> Result<AirStmt> {
        // Convert keyword trap to trap vector
        let trap_vect = match kind {
            TrapKind::Trap => self.expect_lit(Bits::Unsigned(8))?,
            TrapKind::Getc => 0x20,
            TrapKind::Out => 0x21,
            TrapKind::Puts => 0x22,
            TrapKind::In => 0x23,
            TrapKind::Putsp => 0x24,
            TrapKind::Halt => 0x25,
        } as u8;

        Ok(AirStmt::Trap { trap_vect })
    }

    fn parse_byte(&mut self, val: u16) -> AirStmt {
        AirStmt::RawWord {
            bytes: RawWord(val),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token> {
        match self.toks.next() {
            Some(tok) if tok.kind == expected => Ok(tok),
            Some(unexpected) => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_token",
                help = "check the type of operands allowed for this instruction.",
                labels = vec![LabeledSpan::at(unexpected.span, "unexpected token")],
                "Expected token of type {}, found {}",
                expected,
                unexpected.kind
            ),
            None => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_eof",
                help = "You may be missing some instrutions for your last statement.",
                "Unexpected end of file",
            ),
        }
    }

    fn expect_where(
        &mut self,
        mut check: impl FnMut(&TokenKind) -> bool,
        expected: &str,
    ) -> Result<Token> {
        match self.toks.next() {
            Some(tok) if check(&tok.kind) => Ok(tok),
            Some(unexpected) => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_token",
                help = "check the type of operands allowed for this instruction.",
                labels = vec![LabeledSpan::at(unexpected.span, "unexpected token")],
                "Expected token of type {}, found {}",
                expected,
                unexpected.kind
            ),
            None => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_eof",
                help = "You may be missing some instrutions for your last statement.",
                "Unexpected end of file",
            ),
        }
    }

    fn expect_lit(&mut self, bits: Bits) -> Result<u16> {
        let check_range = |val| -> bool {
            match bits {
                Bits::Signed(num_bits) => {
                    let val = val as i16;
                    let range = 2_i16.pow(num_bits as u32 - 1);
                    (-range..range).contains(&val)
                }
                Bits::Unsigned(num_bits) => {
                    let range = 2_u16.pow(num_bits as u32 - 1);
                    (0..range).contains(&val)
                }
            }
        };

        let tok = self.expect_where(
            |kind| {
                matches!(
                    kind,
                    TokenKind::Lit(LiteralKind::Dec(_) | LiteralKind::Hex(_))
                )
            },
            "numeric literal",
        )?;
        let val = match tok.kind {
            TokenKind::Lit(LiteralKind::Dec(val)) => val as u16,
            TokenKind::Lit(LiteralKind::Hex(val)) => val,
            _ => unreachable!(),
        };
        match check_range(val) {
            true => Ok(val),
            false => {
                bail!(
                    severity = Severity::Error,
                    help = format!(
                        "this instruction expects literals that can be contained in {bits} bits",
                    ),
                    labels = vec![LabeledSpan::at(tok.span, "out-of-range literal")],
                    "Found numeric literal {val} of incorrect size"
                )
            }
        }
    }

    fn expect_reg(&mut self) -> Result<Register> {
        match self
            .expect_where(|tok| matches!(tok, TokenKind::Reg(_)), "register")?
            .kind
        {
            TokenKind::Reg(reg) => Ok(reg),
            _ => unreachable!(),
        }
    }

    fn expect_lit_or_reg(&mut self) -> Result<ImmediateOrReg> {
        match self.toks.peek() {
            Some(tok) => match tok.kind {
                TokenKind::Reg(_) => {
                    let reg = self.expect_reg()?;
                    Ok(ImmediateOrReg::Reg(reg))
                }
                TokenKind::Lit(_) => {
                    let val = self.expect_lit(Bits::Signed(5))?;
                    Ok(ImmediateOrReg::Imm5(val as u8))
                }
                unexpected => bail!(
                    severity = Severity::Error,
                    code = "parse::unexpected_token",
                    help = "check the type of operands allowed for this instruction.",
                    labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                    "Expected token of type literal or register, found {}",
                    unexpected
                ),
            },
            None => bail!(
                severity = Severity::Error,
                code = "parse::unexpected_eof",
                help = "You may be missing some instrutions for your last statement.",
                "Unexpected end of file",
            ),
        }
    }
}

// Convenient way to pass around bit limits
pub enum Bits {
    Signed(u8),
    Unsigned(u8),
}

impl Display for Bits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Bits::Signed(val) => val,
            Bits::Unsigned(val) => val,
        };
        f.write_str(&val.to_string())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::{
        air::{AirStmt, AsmLine, ImmediateOrReg},
        lexer::TokenKind,
        symbol::{Flag, Register},
    };

    // .FILL TEST
    #[test]
    fn preproc_fill() {
        let res = preprocess("temp .fill x3000").unwrap();
        assert!(res[1].kind == TokenKind::Byte(0x3000))
    }

    #[test]
    fn preproc_fill_neg() {
        let res = preprocess("temp .fill #-35").unwrap();
        assert!(res[1].kind == TokenKind::Byte(-35i16 as u16))
    }

    #[test]
    fn preproc_fill_dec() {
        let res = preprocess("temp .fill #3500").unwrap();
        assert!(res[1].kind == TokenKind::Byte(3500))
    }

    #[test]
    fn preproc_fill_invalid() {
        assert!(preprocess("temp .fill add").is_err())
    }

    #[test]
    fn preproc_fill_nolabel() {
        assert!(preprocess(".fill x1").is_err())
    }

    // .BLKW TEST
    #[test]
    fn preproc_blkw_basic() {
        let res = preprocess("temp .blkw x2")
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokenKind>>();
        assert!(res[1..] == vec![TokenKind::Byte(0), TokenKind::Byte(0)]);

        let res = preprocess("temp .blkw #3")
            .unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokenKind>>();
        assert!(res[1..] == vec![TokenKind::Byte(0), TokenKind::Byte(0), TokenKind::Byte(0)])
    }

    #[test]
    fn preproc_blkw_neg() {
        assert!(preprocess("temp .blkw #-3").is_err())
    }

    #[test]
    fn preproc_blkw_invalid() {
        assert!(preprocess("temp .blkw add").is_err())
    }

    #[test]
    fn preproc_blkw_nolabel() {
        assert!(preprocess(".blkw #1").is_err())
    }

    // .STRINGZ TEST
    #[test]
    fn preproc_stringz_escaped() {
        // .blkw "\"hello\"\n" => "hello"
        let res = preprocess(r#"temp .stringz "\"hello\n\"""#).unwrap();
        let expected = "\"hello\n\"\0"
            .chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res[1..] == expected)
    }

    #[test]
    fn preproc_stringz_standard() {
        // .blkw "hello" => hello
        let res = preprocess(r#"temp .stringz "hello""#).unwrap();
        let expected = "hello\0"
            .chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res[1..] == expected)
    }

    #[test]
    fn preproc_stringz_invalid() {
        assert!(preprocess(r#"temp .stringz error"#).is_err())
    }

    #[test]
    fn preproc_stringz_nolabel() {
        assert!(preprocess(r#".stringz "error""#).is_err())
    }

    // Regression
    #[test]
    fn preproc_empty_lines() {
        let toks = preprocess(
            r#"
        r0

        r1
        "#,
        )
        .unwrap();
        assert_eq!(toks[0].kind, TokenKind::Reg(Register::R0));
        assert_eq!(toks[1].kind, TokenKind::Reg(Register::R1));
    }

    // Parser tests
    #[test]
    fn parse_add_basic() {
        let parser = AsmParser::new("add r0 r1 r2").unwrap();
        let air = parser.parse().unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Add {
                    dest: Register::R0,
                    src_reg: Register::R1,
                    src_reg_imm: ImmediateOrReg::Reg(Register::R2),
                }
            }
        )
    }

    #[test]
    fn parse_add_imm() {
        let parser = AsmParser::new(
            r#"
        add r0 r1 #15
        add r0 r1 #-16
        "#,
        )
        .unwrap();
        let air = parser.parse().unwrap();
        assert_eq!(air.len(), 2);
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Add {
                    dest: Register::R0,
                    src_reg: Register::R1,
                    src_reg_imm: ImmediateOrReg::Imm5(15),
                }
            }
        );
        assert_eq!(
            air.get(1),
            &AsmLine {
                line: 2,
                stmt: AirStmt::Add {
                    dest: Register::R0,
                    src_reg: Register::R1,
                    src_reg_imm: ImmediateOrReg::Imm5((-16i8) as u8),
                }
            }
        );
    }

    #[test]
    fn parse_add_bad_range() {
        let air = AsmParser::new("add r0 r1 #16").unwrap().parse();
        assert!(air.is_err());
        let air = AsmParser::new("add r0 r1 #-17").unwrap().parse();
        assert!(air.is_err());
    }

    #[test]
    fn parse_branch() {
        let air = AsmParser::new("br label").unwrap().parse().unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Branch {
                    flag: Flag::Nzp,
                    dest_label: Label::empty("label")
                }
            }
        )
    }

    #[test]
    fn parse_fill() {
        let air = AsmParser::new("label .fill x30").unwrap().parse().unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::RawWord {
                    bytes: RawWord(0x30)
                }
            }
        )
    }

    #[test]
    fn parse_stringz() {
        let air = AsmParser::new("label .stringz \"ab\"")
            .unwrap()
            .parse()
            .unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::RawWord {
                    bytes: RawWord('a' as u16)
                }
            }
        );
        assert_eq!(
            air.get(1),
            &AsmLine {
                line: 2,
                stmt: AirStmt::RawWord {
                    bytes: RawWord('b' as u16)
                }
            }
        );
        assert_eq!(
            air.get(2),
            &AsmLine {
                line: 3,
                stmt: AirStmt::RawWord {
                    bytes: RawWord('\0' as u16)
                }
            }
        );
    }

    #[test]
    fn parse_label() {
        let air = AsmParser::new(
            r#"
        label add r0 r0 r0
              br label
              br not_existing
        "#,
        )
        .unwrap()
        .parse()
        .unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Add {
                    dest: Register::R0,
                    src_reg: Register::R0,
                    src_reg_imm: ImmediateOrReg::Reg(Register::R0)
                }
            }
        );
        assert_eq!(
            air.get(1),
            &AsmLine {
                line: 2,
                stmt: AirStmt::Branch {
                    flag: Flag::Nzp,
                    dest_label: Label::dummy(1)
                }
            }
        );
        assert_eq!(
            air.get(2),
            &AsmLine {
                line: 3,
                stmt: AirStmt::Branch {
                    flag: Flag::Nzp,
                    dest_label: Label::empty("not_existing")
                }
            }
        );
    }
}
