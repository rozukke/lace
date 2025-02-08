use super::{error, integer, Label, TryParse};

/// Returns `true` if the given character can appear at the start of a label.
pub fn can_start_with(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}
/// Returns `true` if the given character can appear as a subsequent character of a label.
pub fn can_contain(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

type CharIter<'a> = std::iter::Peekable<std::str::Chars<'a>>;

/// Track byte length of consumed characters.
struct ByteCounted<'a> {
    inner: CharIter<'a>,
    len: usize,
}

impl Iterator for ByteCounted<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.inner.next()?;
        self.len += ch.len_utf8();
        Some(ch)
    }
}

impl<'a> ByteCounted<'a> {
    pub fn from(inner: CharIter<'a>) -> Self {
        Self { inner, len: 0 }
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }
}

impl<'a> TryParse<'a> for Label<'a> {
    fn try_parse(string: &'a str) -> Result<Option<Self>, error::Value> {
        let mut chars = ByteCounted::from(string.chars().peekable());

        // Check first character can begin a label
        if !chars.next().is_some_and(can_start_with) {
            return Ok(None);
        };
        // Take characters until non-alphanumeric
        while chars.peek().copied().is_some_and(can_contain) {
            chars.next();
        }

        let length = chars.len();
        let (name, offset_str) = string.split_at(length);

        let offset = if offset_str.is_empty() {
            0
        } else {
            match integer::parse_integer(offset_str, true)? {
                Some(offset) => offset.as_i16()?,
                None => return Err(error::Value::MalformedLabel {}),
            }
        };

        Ok(Some(Label { name, offset }))
    }
}
