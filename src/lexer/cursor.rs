//! Taken from the lexer in https://github.com/rozukke/mimi
// Heavily instpired and referenced from `rustc_lexer` and adapted to suit the project.
// See https://doc.rust-lang.org/beta/nightly-rustc/src/rustc_lexer/cursor.rs.html

/// Peekable iterator over a char sequence.
pub struct Cursor<'a> {
    len_remaining: usize,
    /// Index that the cursor is pointing to in the source
    curr_pt: usize,
    /// Iterator over chars in a &str
    chars: &'a str,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            len_remaining: input.len(),
            curr_pt: 0,
            chars: input,
        }
    }

    pub fn get_next(&self, len: usize) -> &'a str {
        println!("{}", &self.chars[self.curr_pt..(self.curr_pt + len)]);
        &self.chars[self.curr_pt..(self.curr_pt + len)]
    }

    /// File is finished parsing
    pub fn is_eof(&self) -> bool {
        self.len_remaining == 0
    }

    /// Return slice of input starting at the current point of the cursor
    pub fn at_curr_pt(&self) -> &'a str {
        &self.chars[self.curr_pt..]
    }

    /// Move cursor ahead in the input by given amount
    pub fn advance(&mut self, amt: usize) {
        self.curr_pt += amt;
        self.len_remaining -= amt;
    }

    /// Advance by one character
    pub fn bump(&mut self) {
        self.advance(1)
    }

    /// Returns current cursor position
    pub fn curr_pt(&self) -> usize {
        self.curr_pt
    }
}
