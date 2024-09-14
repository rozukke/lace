//! Heavily inspired and referenced from `rustc_lexer` and adapted to suit the project.
//! See https://doc.rust-lang.org/beta/nightly-rustc/src/rustc_lexer/cursor.rs.html

use std::{ops::Range, str::Chars};

#[derive(Clone)]
/// Peekable iterator over a char sequence.
pub struct Cursor<'sess> {
    len_remaining: usize,
    orig_size: usize,
    /// Iterator over chars in a &str
    chars: Chars<'sess>,
    src: &'static str,
}

pub(crate) const NULL_CHAR: char = '\0';

impl<'sess> Cursor<'sess> {
    pub fn new(src: &'static str) -> Cursor<'sess> {
        Cursor {
            len_remaining: src.len(),
            orig_size: src.len(),
            chars: src.chars(),
            src,
        }
    }

    /// Returns next character without consuming it.
    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(NULL_CHAR)
    }

    pub fn src(&self) -> &'static str {
        self.src
    }

    /// File is finished parsing
    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Advance by one character
    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        Some(c)
    }

    /// Return number of consumed tokens
    pub(crate) fn pos_in_token(&self) -> usize {
        self.len_remaining - self.chars.as_str().len()
    }

    /// Resets the number of consumed chars
    pub(crate) fn reset_pos(&mut self) {
        self.len_remaining = self.chars.as_str().len();
    }

    /// Consume until given function returns false
    pub(crate) fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    pub(crate) fn abs_pos(&self) -> usize {
        self.orig_size - self.len_remaining + self.pos_in_token()
    }

    pub(crate) fn get_range(&self, range: Range<usize>) -> &str {
        &self.src[range]
    }
}
