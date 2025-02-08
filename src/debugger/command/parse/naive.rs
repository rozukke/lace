use std::fmt;

use super::{integer::Radix, label};

#[derive(Debug, PartialEq)]
pub enum NaiveType {
    Integer,
    Register,
    Label,
    PCOffset,
}

impl NaiveType {
    pub fn as_str(&self) -> &'static str {
        match self {
            NaiveType::Integer => "integer",
            NaiveType::Register => "register",
            NaiveType::Label => "label",
            NaiveType::PCOffset => "PC offset",
        }
    }

    /// If string does not start with `[-+#0-9]`, then all characters need to be checked, to ensure
    /// labels starting with `[bBoOxX]` don't get classified as integers.
    ///
    /// - `^[-+#0-9]`
    /// - or `^[bB][-+]?[01]+$`
    /// - or `^[oO][-+]?[0-7]+$`
    /// - or `^[xX][-+]?[0-9a-fA-F]+$`
    fn is_str_integer(string: &str) -> bool {
        let mut chars = string.chars().peekable();
        if chars
            .peek()
            .is_some_and(|ch| matches!(ch, '-' | '+' | '#' | '0'..='9'))
        {
            return true;
        }
        let radix = match chars.next() {
            Some('b' | 'B') => Radix::Binary,
            Some('o' | 'O') => Radix::Octal,
            Some('x' | 'X') => Radix::Hex,
            _ => return false,
        };
        if matches!(chars.peek(), Some('-' | '+')) {
            chars.next();
        }
        if chars.peek().is_none() {
            return false;
        }
        for ch in chars {
            if radix.parse_digit(ch).is_none() {
                return false;
            }
        }
        true
    }

    /// - `^[rR][0-7]$`
    fn is_str_register(string: &str) -> bool {
        let mut chars = string.chars();
        chars.next().is_some_and(|ch| matches!(ch, 'r' | 'R'))
            && chars.next().is_some_and(|ch| matches!(ch, '0'..='7'))
            && !chars.next().is_some_and(label::can_contain)
    }

    /// - `^[a-zA-Z_]
    fn is_str_label(string: &str) -> bool {
        string.chars().next().is_some_and(label::can_start_with)
    }

    /// - `^\^`
    fn is_str_pc_offset(string: &str) -> bool {
        string.chars().next().is_some_and(|ch| ch == '^')
    }
}

impl fmt::Display for NaiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<&str> for NaiveType {
    type Error = ();

    fn try_from(string: &str) -> Result<Self, Self::Error> {
        // Ordered by approximate function speed
        if Self::is_str_pc_offset(string) {
            return Ok(Self::PCOffset);
        }
        if Self::is_str_register(string) {
            return Ok(Self::Register);
        }
        if Self::is_str_integer(string) {
            return Ok(Self::Integer);
        }
        if Self::is_str_label(string) {
            return Ok(Self::Label);
        }
        Err(())
    }
}
