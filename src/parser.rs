use std::{borrow::{Borrow, Cow}, iter::{Filter, Peekable}, ops::RangeBounds, vec::IntoIter};

use miette::{bail, miette, LabeledSpan, Result, Severity};

use crate::{
    lexer::{cursor::Cursor, LiteralKind, Token, TokenKind}, ops::Air, symbol::{DirKind, Span}
};

// TODO: Kind of ugly, see if improvable
fn unescape(s: &str) -> Cow<str> {
    if s.find('\\').is_none() {
        return Cow::Borrowed(s);
    }
    let mut result = String::new();
    let mut chars = s.chars().peekable();
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
                },
                None => {
                    // Trailing backslash; include it as is
                    result.push('\\');
                },
            }
            needs_allocation = true;
        } else {
            result.push(c);
        }
    }
    Cow::Owned(result)
}

/// Replaces raw value directives .fill, .blkw, .stringz with equivalent raw bytes
/// Returns a 'final' vector of tokens. This is easier than working with an iterator that can
/// either return a single token or a Vec of tokens.
pub fn preprocess(src: &str) -> Result<Vec<Token>> {
    let mut res: Vec<Token> = Vec::new();
    let mut cur = Cursor::new(src);

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
                    )
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
                    )
                }
            },
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
                    )

                }
            },
            TokenKind::Comment | TokenKind::Whitespace => continue,
            TokenKind::Eof => break,
            _ => res.push(dir),
        }
    }
    Ok(res)
}

/// Transforms token stream into AIR
pub struct AsmParser<'a> {
    /// Reference to the source file
    src: &'a str,
    /// Peekable iterator over preprocessed tokens
    toks: Peekable<IntoIter<Token>>,
    /// Assembly intermediate representation
    air: Air,
}

impl<'a> AsmParser<'a> {
    /// Takes in preprocessed tokens or will otherwise go into unreachable code. Input should
    /// contain no whitespace or comments.
    pub fn new(src: &'a str, toks: Vec<Token>) -> Self {
        AsmParser {
            src,
            toks: toks.into_iter().peekable(),
            air: Air::new(),
        }
    }

    /// Create AIR out of token stream
    pub fn parse(mut self) -> Result<Air> {
        // Has to take front label => Byte
        // Cannot take label => .orig
        // Can take label => inst, trap

        loop {
            // Might be a better pattern for this
            if self.toks.peek().is_none() {
                break;
            }
            // Can/has to take label: bytes, instr, trap
            if let Some(label) = self.get_label() {
                // Add prefix label to symbol table
                if let Some(tok) = self.toks.next() {
                    match tok.kind {
                        // Invalid after label
                        TokenKind::Label 
                        | TokenKind::Lit(_) 
                        | TokenKind::Reg(_)
                        // This is actually only .orig at this stage
                        | TokenKind::Dir(_) => bail!(
                            severity = Severity::Error,
                            code = "parse::unexpected_token",
                            help = "Labels should be followed by an instruction, trap, or directive.",
                            labels = vec![LabeledSpan::at(tok.span, "unexpected token")],
                            "Unexpected token type: {}", tok.kind
                        ),
                        TokenKind::Instr(_) => self.parse_instr()?,
                        TokenKind::Trap(_) => self.parse_trap()?,
                        TokenKind::Byte(_) => self.parse_byte()?,
                        // Does not exist in preprocessed token stream
                        TokenKind::Whitespace
                        | TokenKind::Comment
                        | TokenKind::Eof => unreachable!(),
                    }
                } else {
                    bail!(
                        severity = Severity::Error,
                        code = "parse::unexpected_eof",
                        help = "Labels should be followed by an instruction, trap, or directive.",
                        labels = vec![LabeledSpan::at_offset(label.span.end(), "unexpected eof")],
                        "Unexpected end of file"
                    )
                }
            }
            // May drop label/has to drop label: .orig, instr, trap
            else {
                if let Some(tok) = self.toks.next() {

                } else {
                    break;
                }
            }
        }

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

        Ok(self.air)
    }

    /// Return label or leave iter untouched and return None
    fn get_label(&mut self) -> Option<Token> {
        if let Some(tok) = self.toks.peek() {
            return match tok.kind {
                TokenKind::Label => {
                    // Guaranteed to be Some
                    Some(self.toks.next().unwrap())
                },
                _ => None,
            }
        }
        None
    }

    /// Process several tokens to form valid instruction AIR
    fn parse_instr(&mut self) -> Result<()> {
        todo!()
    }

    fn parse_trap(&mut self) -> Result<()>{
        todo!()
    }

    fn parse_byte(&mut self) -> Result<()> {
        todo!()
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
        let res = preprocess(".blkw x2").unwrap()
            .iter()
            .map(|tok| tok.kind.clone())
            .collect::<Vec<TokenKind>>();
        assert!(res == vec![TokenKind::Byte(0), TokenKind::Byte(0)]);

        let res = preprocess(".blkw #3").unwrap()
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
        let expected = "\"hello\n\"\0".chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res == expected)
    }

    #[test]
    fn preproc_stringz_standard() {
        // .blkw "hello" => hello
        let res = preprocess(r#".stringz "hello""#).unwrap();
        let expected = "hello\0".chars()
            .map(|c| Token::byte(c as u16))
            .collect::<Vec<Token>>();
        assert!(res == expected)
    }

    #[test]
    fn preproc_stringz_invalid() {
        assert!(preprocess(r#".stringz error"#).is_err())
    }
}
