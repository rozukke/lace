use std::{borrow::Cow, fmt::Display, iter::Peekable, vec::IntoIter};

use miette::Result;

use crate::{
    air::{Air, AirStmt, ImmediateOrReg, RawWord},
    error,
    lexer::{cursor::Cursor, LiteralKind, Token, TokenKind},
    symbol::{DirKind, InstrKind, Label, Register, Span, TrapKind},
};

/// Replaces raw value directives .fill, .blkw, .stringz with equivalent raw bytes
/// Returns a 'final' vector of tokens. This is easier than working with an iterator that can
/// either return a single token or a Vec of tokens.
pub fn preprocess(src: &'static str) -> Result<(Vec<Token>, Vec<u16>)> {
    let mut res: Vec<Token> = Vec::new();
    let mut breakpoints = Vec::new();
    let mut cur = Cursor::new(src);

    loop {
        let dir = cur.advance_real()?;
        match dir.kind {
            // Into raw word with the next literal as value
            TokenKind::Dir(DirKind::Fill) => {
                let val = cur.advance_real()?;
                match val.kind {
                    TokenKind::Lit(LiteralKind::Hex(lit)) => {
                        res.push(Token::byte(lit));
                    }
                    TokenKind::Lit(LiteralKind::Dec(lit)) => {
                        res.push(Token::byte(lit as u16));
                    }
                    _ => return Err(error::preproc_bad_lit(val.span, src, false)),
                }
            }
            // Into a series of raw null words
            TokenKind::Dir(DirKind::Blkw) => {
                let val = cur.advance_real()?;
                match val.kind {
                    TokenKind::Lit(LiteralKind::Hex(lit)) => {
                        for _ in 0..lit {
                            res.push(Token::nullbyte());
                        }
                    }
                    TokenKind::Lit(LiteralKind::Dec(lit)) => {
                        if lit < 0 {
                            println!("{:?}", error::preproc_bad_lit(val.span, src, true));
                        }
                        for _ in 0..lit as u16 {
                            res.push(Token::nullbyte());
                        }
                    }
                    _ => return Err(error::preproc_bad_lit(val.span, src, false)),
                }
            }
            // str into a sequence of bytes corresponding to a literal + null terminator
            TokenKind::Dir(DirKind::Stringz) => {
                let val = cur.advance_real()?;
                match val.kind {
                    TokenKind::Lit(LiteralKind::Str) => {
                        let str_raw = cur.get_range(val.span.into());
                        // Get rid of quotation marks
                        for c in unescape(&str_raw[1..str_raw.len() - 1]).chars() {
                            res.push(Token::byte(c as u16));
                        }
                        res.push(Token::nullbyte());
                    }
                    _ => return Err(error::preproc_no_str(val.span, src)),
                }
            }
            TokenKind::Dir(DirKind::Break) => {
                breakpoints.push(res.len() as u16);
            }
            // Eliminated during preprocessing
            TokenKind::Comment | TokenKind::Whitespace => continue,
            TokenKind::Eof | TokenKind::Dir(DirKind::End) => break,
            _ => res.push(dir),
        }
    }
    Ok((res, breakpoints))
}

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
                    // Trailing \
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
pub struct AsmParser {
    /// Reference to the source file
    src: &'static str,
    /// Peekable iterator over preprocessed tokens
    toks: Peekable<IntoIter<Token>>,
    /// Assembly intermediate representation
    air: Air,
    /// Tracker for current line
    line: u16,
}

impl AsmParser {
    /// Preprocesses tokens, otherwise will go into unreachable code. Input should
    /// contain no whitespace or comments.
    pub fn new(src: &'static str) -> Result<Self> {
        let (toks, breakpoints) = preprocess(src)?;
        Ok(AsmParser {
            src,
            toks: toks.into_iter().peekable(),
            air: Air::new(breakpoints),
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
                    Err(_) => return Err(error::parse_duplicate_label(label.span, self.src)),
                }
            }

            // Parse line
            if let Some(tok) = self.toks.next() {
                let stmt = match tok.kind {
                    // Lines should not start with these tokens
                    TokenKind::Label | TokenKind::Lit(_) | TokenKind::Reg(_) => {
                        return Err(error::parse_generic_unexpected(
                            self.src,
                            "directive/instrucion/trap",
                            tok,
                        ))
                    }
                    TokenKind::Dir(dir) => {
                        assert!(dir == DirKind::Orig);
                        let orig = self.expect_lit(Bits::Unsigned(16))?;
                        self.air.set_orig(orig)?;
                        continue;
                    }
                    TokenKind::Instr(instr_kind) => self.parse_instr(instr_kind)?,
                    TokenKind::Trap(trap_kind) => self.parse_trap(trap_kind)?,
                    TokenKind::Byte(val) => self.parse_byte(val),
                    // Does not exist in preprocessed token stream
                    TokenKind::Whitespace | TokenKind::Comment | TokenKind::Eof => {
                        unreachable!("Found whitespace/comment/eof in preprocessed stream")
                    }
                };
                self.air.add_stmt(stmt);
            } else {
                if labeled_line {
                    return Err(error::parse_eof(self.src));
                }
                break;
            }

            self.line += 1;
        }
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
            InstrKind::Push => {
                let src_reg = self.expect_reg()?;
                Ok(AirStmt::Push { src_reg })
            }
            InstrKind::Pop => {
                let dest_reg = self.expect_reg()?;
                Ok(AirStmt::Pop { dest_reg })
            }
            InstrKind::Call => {
                let label_tok = self.expect(TokenKind::Label)?;
                let dest_label = Label::try_fill(self.get_span(label_tok.span));
                Ok(AirStmt::Call { dest_label })
            }
            InstrKind::Rets => Ok(AirStmt::Rets),
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
            InstrKind::Str => {
                let src_reg = self.expect_reg()?;
                let dest_reg = self.expect_reg()?;
                let offset = self.expect_lit(Bits::Signed(6))? as u8;
                Ok(AirStmt::StoreOffs {
                    src_reg,
                    dest_reg,
                    offset,
                })
            }
        }
    }

    /// Convert keyword trap to predetermined trap vector
    fn parse_trap(&mut self, kind: TrapKind) -> Result<AirStmt> {
        let trap_vect = match kind {
            TrapKind::Generic => self.expect_lit(Bits::Unsigned(8))?,
            TrapKind::Getc => 0x20,
            TrapKind::Out => 0x21,
            TrapKind::Puts => 0x22,
            TrapKind::In => 0x23,
            TrapKind::Putsp => 0x24,
            TrapKind::Halt => 0x25,
            TrapKind::Putn => 0x26,
            TrapKind::Reg => 0x27,
        } as u8;

        Ok(AirStmt::Trap { trap_vect })
    }

    fn parse_byte(&mut self, val: u16) -> AirStmt {
        AirStmt::RawWord { val: RawWord(val) }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token> {
        match self.toks.next() {
            Some(tok) if tok.kind == expected => Ok(tok),
            Some(unexpected) => {
                return Err(error::parse_generic_unexpected(
                    self.src,
                    format!("{expected}").as_str(),
                    unexpected,
                ))
            }
            None => return Err(error::parse_eof(self.src)),
        }
    }

    fn expect_where(
        &mut self,
        mut check: impl FnMut(&TokenKind) -> bool,
        expected: &str,
    ) -> Result<Token> {
        match self.toks.next() {
            Some(tok) if check(&tok.kind) => Ok(tok),
            Some(unexpected) => {
                return Err(error::parse_generic_unexpected(
                    self.src, expected, unexpected,
                ))
            }
            None => return Err(error::parse_eof(self.src)),
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
            _ => unreachable!("Found non-literal after checking for literal type"),
        };
        match check_range(val) {
            true => Ok(val),
            false => Err(error::parse_lit_range(tok.span, self.src, bits)),
        }
    }

    fn expect_reg(&mut self) -> Result<Register> {
        match self
            .expect_where(|tok| matches!(tok, TokenKind::Reg(_)), "register")?
            .kind
        {
            TokenKind::Reg(reg) => Ok(reg),
            _ => unreachable!("Found non-reg after filtering for reg"),
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
                _ => {
                    return Err(error::parse_generic_unexpected(
                        self.src,
                        "literal or register",
                        *tok,
                    ))
                }
            },
            None => return Err(error::parse_eof(self.src)),
        }
    }
}

/// Convenient way to pass around bit limits
#[derive(Debug)]
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
        let res = preprocess(".fill x1").unwrap();
        assert!(res[0].kind == TokenKind::Byte(1))
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
        // TODO: potentially test for warnings
        assert!(preprocess("temp .blkw #-3").is_ok())
    }

    #[test]
    fn preproc_blkw_invalid() {
        assert!(preprocess("temp .blkw add").is_err())
    }

    #[test]
    fn preproc_blkw_nolabel() {
        let res = preprocess(".blkw #1").unwrap();
        assert!(res[0].kind == TokenKind::Byte(0))
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
        let res = preprocess(r#".stringz "ok""#).unwrap();
        assert!(res[0].kind == TokenKind::Byte('o' as u16));
        assert!(res[1].kind == TokenKind::Byte('k' as u16));
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
                stmt: AirStmt::RawWord { val: RawWord(0x30) }
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
                    val: RawWord('a' as u16)
                }
            }
        );
        assert_eq!(
            air.get(1),
            &AsmLine {
                line: 2,
                stmt: AirStmt::RawWord {
                    val: RawWord('b' as u16)
                }
            }
        );
        assert_eq!(
            air.get(2),
            &AsmLine {
                line: 3,
                stmt: AirStmt::RawWord {
                    val: RawWord('\0' as u16)
                }
            }
        );
    }

    #[test]
    fn parse_stringz_mult() {
        let air = AsmParser::new(
            r#"
        .stringz "a"
        .stringz "b"
        "#,
        )
        .unwrap()
        .parse()
        .unwrap();
        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::RawWord {
                    val: RawWord('a' as u16)
                }
            }
        );
        assert_eq!(
            air.get(2),
            &AsmLine {
                line: 3,
                stmt: AirStmt::RawWord {
                    val: RawWord('b' as u16)
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
