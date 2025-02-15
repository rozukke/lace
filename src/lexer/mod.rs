use std::fmt::Display;
use std::num::IntErrorKind;
use std::str::FromStr;
use std::{i16, u16};

use miette::Result;

use crate::lexer::cursor::Cursor;
use crate::symbol::{DirKind, Flag, InstrKind, Register, Span, SrcOffset, TrapKind};
use crate::{error, features};

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
    /// Preprocessor raw values
    Byte(u16),
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
            TokenKind::Whitespace | TokenKind::Comment | TokenKind::Eof | TokenKind::Byte(_) => {
                unreachable!("whitespace, comment, eof, byte attempted to be displayed")
            }
        };
        f.write_str(lit)
    }
}

#[allow(dead_code)]
pub fn tokenize(input: &'static str) -> impl Iterator<Item = Result<Token>> + '_ {
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

/// Test if a character is considered to be whitespace, including commas.
pub(crate) fn is_whitespace(c: char) -> bool {
    char::is_ascii_whitespace(&c) || matches!(c, ',' | ':')
}

pub(crate) fn is_reg_num(c: char) -> bool {
    // R0 - R7
    matches!(c, '0'..='7')
}

/// Test if a character is considered an LC3 identifier character.
pub(crate) fn is_id(c: char) -> bool {
    // Non-prefixed numerical literals are considered identifiers.
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
                _ => self.ident()?,
            },
            // Register literals
            'r' | 'R' => match self.first() {
                c if is_reg_num(c) => {
                    self.take_while(is_reg_num);
                    if self.pos_in_token() == 2
                        && match self.first() {
                            c if is_whitespace(c) => true,
                            '\0' => true,
                            _ => false,
                        }
                    {
                        // SAFETY: c is always valid
                        TokenKind::Reg(Register::from_str(&c.to_string()).unwrap())
                    } else {
                        self.ident()?
                    }
                }
                _ => self.ident()?,
            },
            // Check only after other identifier-likes
            c if is_id(c) => self.ident()?,
            // Decimal literal
            '#' => self.dec()?,
            // Directive
            '.' => self.dir()?,
            // String literal
            '"' => self.str()?,
            // Unknown starting characters
            _ => {
                let start = self.abs_pos() - 1;
                self.take_while(|c| !is_whitespace(c));
                return Err(error::lex_unknown(
                    (start..self.abs_pos()).into(),
                    self.src(),
                ));
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
        let value = match i16::from_str_radix(str_val, 16) {
            Ok(value) => value as u16,
            Err(_) => match u16::from_str_radix(str_val, 16) {
                Ok(value) => value,
                Err(e) => match e.kind() {
                    IntErrorKind::PosOverflow => {
                        return Err(error::lex_invalid_lit(
                            (start - prefix..self.abs_pos()).into(),
                            self.src(),
                            e,
                        ))
                    }
                    _ => return Ok(self.ident()?),
                },
            },
        };

        Ok(TokenKind::Lit(LiteralKind::Hex(value)))
    }

    fn dec(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        self.take_while(|c| !is_whitespace(c));
        let str_val = self.get_range(start..self.abs_pos());

        // i16 to handle negative values
        let value = match i16::from_str_radix(&str_val, 10) {
            Ok(value) => value,
            Err(_) => match u16::from_str_radix(&str_val, 10) {
                Ok(val) => val as i16,
                Err(e) => {
                    return Err(error::lex_invalid_lit(
                        (start - prefix..self.abs_pos()).into(),
                        self.src(),
                        e,
                    ))
                }
            },
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
            if c == '\\' {
                self.bump();
            }
        }
        if !terminated {
            return Err(error::lex_unclosed_str(
                (start..self.abs_pos()).into(),
                self.src(),
            ));
        }
        Ok(TokenKind::Lit(LiteralKind::Str))
    }

    fn dir(&mut self) -> Result<TokenKind> {
        // Starting .
        let start = self.abs_pos() - 1;
        self.take_while(is_id);
        let dir = self.get_range(start..self.abs_pos()).to_ascii_lowercase();

        if let Some(token_kind) = self.check_directive(&dir) {
            Ok(token_kind)
        } else {
            Err(error::lex_invalid_dir(
                (start..self.abs_pos()).into(),
                self.src(),
            ))
        }
    }

    fn ident(&mut self) -> Result<TokenKind> {
        let ident_start = self.abs_pos() - 1;
        self.take_while(is_id);
        let ident = self
            .get_range(ident_start..self.abs_pos())
            .to_ascii_lowercase();

        let mut token_kind = self.check_instruction(&ident, ident_start)?;
        if token_kind == TokenKind::Label {
            token_kind = self.check_trap(&ident);
        }
        Ok(token_kind)
    }

    /// Expects lowercase
    fn check_directive(&self, dir_str: &str) -> Option<TokenKind> {
        use DirKind::*;
        use TokenKind::Dir;
        match dir_str {
            ".orig" => Some(Dir(Orig)),
            ".end" => Some(Dir(End)),
            ".stringz" => Some(Dir(Stringz)),
            ".blkw" => Some(Dir(Blkw)),
            ".fill" => Some(Dir(Fill)),
            _ => None,
        }
    }

    /// Expects lowercase
    fn check_instruction(&self, ident: &str, start_pos: usize) -> Result<TokenKind> {
        use InstrKind::*;
        use TokenKind::Instr;

        if matches!(ident, "pop" | "push" | "call" | "rets") {
            if !features::stack() {
                return Err(error::lex_stack_extension_not_enabled(
                    ident,
                    Span::new(SrcOffset(start_pos), self.pos_in_token()),
                    self.src(),
                ));
            }
        }

        Ok(match ident {
            "add" => Instr(Add),
            "and" => Instr(And),
            "br" => Instr(Br(Flag::Nzp)),
            "brnzp" => Instr(Br(Flag::Nzp)),
            "brnz" => Instr(Br(Flag::Nz)),
            "brzp" => Instr(Br(Flag::Zp)),
            "brnp" => Instr(Br(Flag::Np)),
            "brn" => Instr(Br(Flag::N)),
            "brz" => Instr(Br(Flag::Z)),
            "brp" => Instr(Br(Flag::P)),
            "jmp" => Instr(Jmp),
            "jsr" => Instr(Jsr),
            "jsrr" => Instr(Jsrr),
            "ld" => Instr(Ld),
            "ldi" => Instr(Ldi),
            "ldr" => Instr(Ldr),
            "lea" => Instr(Lea),
            "not" => Instr(Not),
            "ret" => Instr(Ret),
            "rti" => Instr(Rti),
            "st" => Instr(St),
            "sti" => Instr(Sti),
            "str" => Instr(Str),
            "pop" => Instr(Pop),
            "push" => Instr(Push),
            "call" => Instr(Call),
            "rets" => Instr(Rets),
            _ => TokenKind::Label,
        })
    }

    /// Expects lowercase
    fn check_trap(&self, ident: &str) -> TokenKind {
        use TokenKind::Trap;
        use TrapKind::*;
        match ident {
            "trap" => Trap(Generic),
            "getc" => Trap(Getc),
            "out" => Trap(Out),
            "puts" => Trap(Puts),
            "in" => Trap(In),
            "putsp" => Trap(Putsp),
            "halt" => Trap(Halt),
            "putn" => Trap(Putn),
            "reg" => Trap(Reg),
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
        assert_eq!(res.kind, TokenKind::Lit(LiteralKind::Hex(0x1234)))
    }

    #[test]
    fn hex_too_large() {
        let mut lex = Cursor::new("xFFFF x10000");
        let res = lex.advance_token().unwrap();
        assert_eq!(res.kind, TokenKind::Lit(LiteralKind::Hex(0xFFFF)));
        // Whitespace
        assert!(lex.advance_real().is_err());
    }

    #[test]
    fn hex_neg() {
        let mut lex = Cursor::new("x-4");
        let res = lex.advance_token().unwrap();
        assert_eq!(res.kind, TokenKind::Lit(LiteralKind::Hex((-0x4i16) as u16)))
    }

    #[test]
    fn hex_leading_0() {
        let mut lex = Cursor::new("0x3000");
        let res = lex.advance_token().unwrap();
        assert_eq!(res.kind, TokenKind::Lit(LiteralKind::Hex(0x3000)))
    }

    #[test]
    fn hex_not_num() {
        let mut lex = Cursor::new("X_S");
        let res = lex.advance_token().unwrap();
        assert_eq!(res.kind, TokenKind::Label)
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
        let mut lex = Cursor::new("#65535 #65536");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec((65535 as u16) as i16)));
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
