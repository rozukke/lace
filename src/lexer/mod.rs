use std::str::FromStr;

use miette::{Result, bail, miette, LabeledSpan, Severity};

use crate::lexer::cursor::Cursor;
use crate::symbol::{DirKind, Flag, InstrKind, Register, Span, SrcOffset, TrapKind};

pub mod cursor;

/// Carries all literal info alongside span location inside source code.
#[derive(Debug)]
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
    /// Also includes commas
    Whitespace,
    Unknown,
    Comment,
    Eof,
}

/// Not actually used in parsing, more for debug purposes.
pub fn tokenize(input: &str) -> impl Iterator<Item = Result<Token>> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        loop {
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
        }
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
                    },
                _ => self.ident(),
            },
            // Register literal
            'r' | 'R' => match self.first() {
                c if is_reg_num(c) => {
                    self.take_while(is_reg_num);
                    // Registers are 2 tokens long and followed by whitespace/comma
                    if self.pos_in_token() == 2 && is_whitespace(self.first()) {
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
                TokenKind::Unknown
            },
        };
        let res = Token::new(token_kind, Span::new(SrcOffset(start_pos), self.pos_in_token()));
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
                return Err(miette!(
                    severity = Severity::Error,
                    code = "parse::hex_lit",
                    help = "only use characters 0-9 and a-F.",
                    labels = vec![LabeledSpan::at(start - prefix..self.abs_pos(), "incorrect literal")],
                    "Encountered an invalid hex literal: {e}",
                ))
            }
        };

        Ok(TokenKind::Lit(LiteralKind::Hex(value)))
    }

    fn dec(&mut self) -> Result<TokenKind> {
        let start = self.abs_pos();
        let prefix = self.pos_in_token();
        // Check for negative sign
        let is_negative = if self.first() == '-' {
            self.bump(); // Skip the negative sign
            true
        } else {
            false
        };
        // Take the numeric part
        self.take_while(|c| char::is_ascii_digit(&c));
        let str_val = self.get_range(start..self.abs_pos());

        // Parse the string as an i16 to handle negative values
        let value = match i16::from_str_radix(&str_val, 10) {
            Ok(value) => value,
            Err(e) => {
                bail!(
                    severity = Severity::Error,
                    code = "parse::dec_lit",
                    help = "LC3 supports 16 bits of space, from -32,768 to 32,767.",
                    labels = vec![LabeledSpan::at(start - prefix..self.abs_pos(), "incorrect literal")],
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
            if c == '\n' {break};
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
        let mut token_kind = TokenKind::Label;
        let ident_start = self.abs_pos() - 1;
        self.take_while(is_id);
        let ident = self.get_range(ident_start..self.abs_pos()).to_ascii_lowercase();

        token_kind = self.check_instruction(&ident);
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
        match ident {
            "add" => TokenKind::Instr(InstrKind::Add),
            "and" => TokenKind::Instr(InstrKind::And),
            "brnzp" => TokenKind::Instr(InstrKind::Br(Flag::Nzp)),
            "brnz" => TokenKind::Instr(InstrKind::Br(Flag::Nz)),
            "brzp" => TokenKind::Instr(InstrKind::Br(Flag::Zp)),
            "brnp" => TokenKind::Instr(InstrKind::Br(Flag::Np)),
            "brn" => TokenKind::Instr(InstrKind::Br(Flag::N)),
            "brz" => TokenKind::Instr(InstrKind::Br(Flag::Z)),
            "brp" => TokenKind::Instr(InstrKind::Br(Flag::P)),
            "jmp" => TokenKind::Instr(InstrKind::Jmp),
            "jsr" => TokenKind::Instr(InstrKind::Jsr),
            "jsrr" => TokenKind::Instr(InstrKind::Jsrr),
            "ld" => TokenKind::Instr(InstrKind::Ld),
            "ldi" => TokenKind::Instr(InstrKind::Ldi),
            "ldr" => TokenKind::Instr(InstrKind::Ldr),
            "lea" => TokenKind::Instr(InstrKind::Lea),
            "not" => TokenKind::Instr(InstrKind::Not),
            "ret" => TokenKind::Instr(InstrKind::Ret),
            "rti" => TokenKind::Instr(InstrKind::Rti),
            "st" => TokenKind::Instr(InstrKind::St),
            "sti" => TokenKind::Instr(InstrKind::Sti),
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
            "trap" => TokenKind::Trap(TrapKind::Generic),
            // Not a trap
            _ => TokenKind::Label,
        }
    }
}

mod tests {
    use crate::lexer::{LiteralKind, TokenKind};

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
        let res = lex.advance_token().unwrap();
        assert!(lex.advance_token().is_err());
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
    fn dec_negative_value () {
        let mut lex = Cursor::new("#-300");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(-300)))
    }

    #[test]
    fn dec_too_small () {
        let mut lex = Cursor::new("#-32768 #-32769");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(-32768)));
        // Whitespace
        let res = lex.advance_token().unwrap();
        assert!(lex.advance_token().is_err());
    }

    #[test]
    fn dec_too_large () {
        let mut lex = Cursor::new("#32767 #32768");
        let res = lex.advance_token().unwrap();
        assert!(res.kind == TokenKind::Lit(LiteralKind::Dec(32767)));
        // Whitespace
        let res = lex.advance_token().unwrap();
        assert!(lex.advance_token().is_err());
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
}
