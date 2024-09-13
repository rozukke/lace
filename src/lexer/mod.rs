use std::fmt::Display;
use std::str::FromStr;

use miette::{bail, LabeledSpan, Result, Severity};

use crate::lexer::cursor::Cursor;
use crate::symbol::{DirKind, Flag, InstrKind, Register, Span, SrcOffset, TrapKind};

pub mod cursor;

/// Carries all literal info alongside span location inside source code.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    /// Lexed token kind, with literal values contained as part of the enum.
    pub kind: TokenKind,
    /// Span pointing at the location of the token in the source.
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn byte(val: u16) -> Self {
        Token {
            kind: TokenKind::Byte(val),
            span: Span::dummy(),
        }
    }

    pub fn nullbyte() -> Self {
        Token::byte(0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LiteralKind {
    /// 0x3000, xFFFF, x123
    Hex(u16),
    /// #-1, #32456
    Dec(i16),
    /// "str with \" escaped chars"
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
    /// Used to represent preprocessor raw values
    Byte(u16),
    /// Also includes commas
    Whitespace,
    Comment,
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lit = match self {
            TokenKind::Label => "label",
            TokenKind::Instr(_) => "instruction",
            TokenKind::Trap(_) => "trap",
            TokenKind::Lit(_) => "literal",
            TokenKind::Dir(_) => "preprocessor directive",
            TokenKind::Reg(_) => "register",
            // Should never be displayed as part of an error
            TokenKind::Whitespace | TokenKind::Comment | TokenKind::Eof | TokenKind::Byte(_) => {
                unreachable!()
            }
        };
        f.write_str(lit)
    }
}

#[allow(dead_code)]
/// Not actually used in parsing, more for debug purposes.
pub fn tokenize(input: &str) -> impl Iterator<Item = Result<Token>> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || loop {
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
    // TODO: ugly
    pub fn advance_real(&mut self) -> Result<Token> {
        match self.advance_token() {
            Ok(tok) if tok.kind == TokenKind::Whitespace => self.advance_token(),
            Ok(tok) => Ok(tok),
            Err(err) => Err(err),
        }
    }

    pub fn advance_token(&mut self) -> Result<Token> {
        let start_pos = self.abs_pos();
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Ok(Token::new(TokenKind::Eof, Span::dummy())),
        };
        let token_kind = match first_char {
            // Comment
            ';' => {
                self.take_while(|c| c != '\n');
                TokenKind::Comment
            }
            // Whitespace
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
                }
                _ => self.ident(),
            },
            // Register literal
            'r' | 'R' => match self.first() {
                c if is_reg_num(c) => {
                    self.take_while(is_reg_num);
                    // Registers are 2 tokens long and followed by whitespace/comma
                    if self.pos_in_token() == 2
                        && match self.first() {
                            c if is_whitespace(c) => true,
                            '\0' => true,
                            _ => false,
                        }
                    {
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
            '.' => self.dir()?,
            // String literal
            '"' => self.str()?,
            // Unknown starting characters
            _ => {
                self.take_while(|c| !is_whitespace(c));
                bail!(
                    severity = Severity::Error,
                    code = "lex::unknown",
                    help = "Check the manual or something...",
                    labels = vec![LabeledSpan::at_offset(start_pos, "unknown token")],
                    "Encounetered an unknown token",
                )
            }
        };
        let res = Token::new(
            token_kind,
            Span::new(SrcOffset(start_pos), self.pos_in_token()),
        );
        self.reset_pos();
        Ok(res)
    }

    fn hex(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        self.take_while(|c| !is_whitespace(c));
        let str_val = self.get_range(start..self.abs_pos());
        let value = match u16::from_str_radix(str_val, 16) {
            Ok(value) => value,
            Err(e) => {
                bail!(
                    severity = Severity::Error,
                    code = "parse::hex_lit",
                    help = "only use characters 0-9 and a-F.",
                    labels = vec![LabeledSpan::at(
                        start - prefix..self.abs_pos(),
                        "incorrect literal"
                    )],
                    "Encountered an invalid hex literal: {e}",
                )
            }
        };

        Ok(TokenKind::Lit(LiteralKind::Hex(value)))
    }

    fn dec(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        // Take the numeric part
        self.take_while(|c| char::is_ascii_digit(&c) || c == '-');
        let str_val = self.get_range(start..self.abs_pos());

        // Parse the string as an i16 to handle negative values
        let value = match i16::from_str_radix(&str_val, 10) {
            Ok(value) => value,
            Err(e) => {
                bail!(
                    severity = Severity::Error,
                    code = "parse::dec_lit",
                    help = "LC3 supports 16 bits of space, from -32,768 to 32,767.",
                    labels = vec![LabeledSpan::at(
                        start - prefix..self.abs_pos(),
                        "incorrect literal"
                    )],
                    "Encountered an invalid decimal literal: {e}",
                )
            }
        };

        Ok(TokenKind::Lit(LiteralKind::Dec(value)))
    }

    fn str(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos() - 1;
        let mut terminated = false;
        while let Some(c) = self.bump() {
            if c == '\n' {
                break;
            };
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

    fn dir(&mut self) -> Result<TokenKind> {
        // Account for starting .
        let start = self.abs_pos() - 1;
        self.take_while(is_id);
        let dir = self.get_range(start..self.abs_pos()).to_ascii_lowercase();

        if let Some(token_kind) = self.check_directive(&dir) {
            Ok(token_kind)
        } else {
            bail!(
                severity = Severity::Error,
                code = "parse::dir",
                help = "hint: check the list of available directives in the documentation.",
                labels = vec![LabeledSpan::at(start..self.abs_pos(), "incorrect literal")],
                "Encountered an invalid directive.",
            )
        }
    }

    fn ident(&mut self) -> TokenKind {
        let ident_start = self.abs_pos() - 1;
        self.take_while(is_id);
        let ident = self
            .get_range(ident_start..self.abs_pos())
            .to_ascii_lowercase();

        let mut token_kind = self.check_instruction(&ident);
        // If not an instruction, check if it's a trap
        if token_kind == TokenKind::Label {
            token_kind = self.check_trap(&ident);
        }

        token_kind
    }

    /// Expects lowercase
    fn check_directive(&self, dir_str: &str) -> Option<TokenKind> {
        match dir_str {
            ".orig" => Some(TokenKind::Dir(DirKind::Orig)),
            ".end" => Some(TokenKind::Dir(DirKind::End)),
            ".stringz" => Some(TokenKind::Dir(DirKind::Stringz)),
            ".blkw" => Some(TokenKind::Dir(DirKind::Blkw)),
            ".fill" => Some(TokenKind::Dir(DirKind::Fill)),
            // Not a directive
            _ => None,
        }
    }

    // Should learn how to write macros tbh :)
    /// Expects lowercase
    fn check_instruction(&self, ident: &str) -> TokenKind {
        use TokenKind::Instr;
        match ident {
            "add" => Instr(InstrKind::Add),
            "and" => Instr(InstrKind::And),
            "br" => Instr(InstrKind::Br(Flag::Nzp)),
            "brnzp" => Instr(InstrKind::Br(Flag::Nzp)),
            "brnz" => Instr(InstrKind::Br(Flag::Nz)),
            "brzp" => Instr(InstrKind::Br(Flag::Zp)),
            "brnp" => Instr(InstrKind::Br(Flag::Np)),
            "brn" => Instr(InstrKind::Br(Flag::N)),
            "brz" => Instr(InstrKind::Br(Flag::Z)),
            "brp" => Instr(InstrKind::Br(Flag::P)),
            "jmp" => Instr(InstrKind::Jmp),
            "jsr" => Instr(InstrKind::Jsr),
            "jsrr" => Instr(InstrKind::Jsrr),
            "ld" => Instr(InstrKind::Ld),
            "ldi" => Instr(InstrKind::Ldi),
            "ldr" => Instr(InstrKind::Ldr),
            "lea" => Instr(InstrKind::Lea),
            "not" => Instr(InstrKind::Not),
            "ret" => Instr(InstrKind::Ret),
            "rti" => Instr(InstrKind::Rti),
            "st" => Instr(InstrKind::St),
            "sti" => Instr(InstrKind::Sti),
            // Not an instruction
            _ => TokenKind::Label,
        }
    }

    /// Expects lowercase
    fn check_trap(&self, ident: &str) -> TokenKind {
        match ident {
            "getc" => TokenKind::Trap(TrapKind::Getc),
            "out" => TokenKind::Trap(TrapKind::Out),
            "puts" => TokenKind::Trap(TrapKind::Puts),
            "in" => TokenKind::Trap(TrapKind::In),
            "putsp" => TokenKind::Trap(TrapKind::Putsp),
            "halt" => TokenKind::Trap(TrapKind::Halt),
            "trap" => TokenKind::Trap(TrapKind::Trap),
            // Not a trap
            _ => TokenKind::Label,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::{LiteralKind, TokenKind},
        symbol::Register,
    };

    use super::cursor::Cursor;

    // HEX LIT TESTS

    #[test]
    fn hex_correct_value() {
        let mut lex = Cursor::new("0x1234");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Hex(0x1234)))
    }

    #[test]
    fn hex_too_large() {
        let mut lex = Cursor::new("xFFFF x10000");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Hex(0xFFFF)));
        // Whitespace
        assert!(lex.advance_real().is_err());
    }

    #[test]
    fn hex_leading_0() {
        let mut lex = Cursor::new("0x3000");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Hex(0x3000)))
    }

    // DEC LIT TESTS

    #[test]
    fn dec_correct_value() {
        let mut lex = Cursor::new("#32412");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(32412)))
    }

    #[test]
    fn dec_negative_value() {
        let mut lex = Cursor::new("#-300");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(-300)))
    }

    #[test]
    fn dec_too_small() {
        let mut lex = Cursor::new("#-32768 #-32769");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(-32768)));
        // Whitespace
        assert!(lex.advance_real().is_err());
    }

    #[test]
    fn dec_too_large() {
        let mut lex = Cursor::new("#32767 #32768");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(32767)));
        // Whitespace
        assert!(lex.advance_real().is_err());
    }

    // STR LIT TESTS

    #[test]
    fn str_unterminated() {
        let mut lex = Cursor::new(r#""unterminated"#);
        assert!(lex.advance_token().is_err())
    }

    #[test]
    fn str_escaped() {
        let mut lex = Cursor::new(r#"there is an escaped \" in this str\n"#);
        assert!(lex.advance_token().is_ok())
    }

    // Regression
    #[test]
    fn registers() {
        let mut lex = Cursor::new("r0 r1 r7 R0 R1 R7");
        assert_eq!(
            lex.advance_token().unwrap().kind,
            TokenKind::Reg(Register::R0)
        );
        assert_eq!(
            lex.advance_real().unwrap().kind,
            TokenKind::Reg(Register::R1)
        );
        assert_eq!(
            lex.advance_real().unwrap().kind,
            TokenKind::Reg(Register::R7)
        );
        assert_eq!(
            lex.advance_real().unwrap().kind,
            TokenKind::Reg(Register::R0)
        );
        assert_eq!(
            lex.advance_real().unwrap().kind,
            TokenKind::Reg(Register::R1)
        );
        assert_eq!(
            lex.advance_real().unwrap().kind,
            TokenKind::Reg(Register::R7)
        );
    }

    #[test]
    fn empty_line() {
        let mut lex = Cursor::new(
            r#"
        r0

        r1
        "#,
        );
        assert_eq!(lex.advance_token().unwrap().kind, TokenKind::Whitespace);
        assert_eq!(
            lex.advance_token().unwrap().kind,
            TokenKind::Reg(Register::R0)
        );
        assert_eq!(lex.advance_token().unwrap().kind, TokenKind::Whitespace);
        assert_eq!(
            lex.advance_token().unwrap().kind,
            TokenKind::Reg(Register::R1)
        );
        assert_eq!(lex.advance_token().unwrap().kind, TokenKind::Whitespace);
    }
}
