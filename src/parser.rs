use std::borrow::{Borrow, Cow};

use miette::{bail, miette, LabeledSpan, Result, Severity};

use crate::{
    lexer::{cursor::Cursor, LiteralKind, Token, TokenKind}, symbol::{DirKind, Span}}
;

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

// TODO: figure out how to handle .orig
// Might require a wrapper struct to pass into this that then gets filled out over several passes

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
            TokenKind::Eof => break,
            _ => res.push(dir),
        }
    }

    Ok(res)
}

// /// Transforms token stream into 'AST'
// pub struct AsmParser<'a> {
//     /// Reference to the source file
//     src: &'a str,
//     /// List of processed tokens
//     tok: Vec<Token>,
//     /// Used to parse tokens
//     cur: Cursor<'a>,
// }

// impl<'a> From<&'a str> for AsmParser<'a> {
//     fn from(src: &'a str) -> Self {
//         let tok: Vec<Token> = StrParser::new(src).proc_tokens();
//         AsmParser {
//             src,
//             tok,
//             cur: Cursor::new(src),
//         }
//     }
// }

// impl<'a> AsmParser<'a> {
//     pub fn parse(&mut self) -> Result<()> {
//         // First, check that there is an .orig directive with an appropriate value.
//         // Should emit error with a label to the first line stating "Expected memory init"
//         // Should be in a function that is also used to init the memory - the question is
//         // whether it should remain as a full directive or as a value that gets emitted afterwards.
//         let orig = self.expect(LTokenKind::Direc)?;
//         // Need ability to expect an enum without specifying a subcase (maybe ()?)
//         let addr = self.expect(LTokenKind::Lit(crate::lexer::LiteralKind::Hex));

//         // Following this, the structure is always:
//         // [label]
//         // ->   <inst> [args]
//         // OR
//         // <label>
//         // ->   <direc> [args]
//         // OR
//         // [label]
//         // ->*   <direc> <args>
//         // OR
//         // <trap> [arg]
//         // or: (sometimes opt label) num directives (opt argument)
//         // so should generally build to this structure. This means, however, that the complexity
//         // is not suuper high as there are really only two medium complexity subcases to parse.
//         //
//         // TODO: Split into LexToken and Token, to simplify the lexer and have a postprocessing
//         // step that can then put it into a Token format that is then easily transformed into
//         // the 'AST'.
//         //
//         // In order to do this, there needs to be peeking functionality on the token stream so
//         // that it can e.g. see if there is a label present at the start of a line.

//         Ok(())
//     }

//     pub fn expect(&mut self, kind: LTokenKind) -> Result<LToken> {
//         let tok = self.cur.advance_token();
//         if tok.kind == kind {
//             return Ok(tok);
//         }
//         Err(miette!(
//             "ParseError: expected token of type {:?}, found {:?}",
//             kind,
//             tok
//         ))
//     }

//     pub fn parse_direc(&self) {
//         todo!()
//     }

//     pub fn parse_op(&self) {
//         todo!()
//     }
// }

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
