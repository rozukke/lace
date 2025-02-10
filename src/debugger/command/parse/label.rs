use super::{error, integer::Integer, CharIter, Label, TryParse};

/// Returns `true` if the given character can appear at the start of a label.
pub fn can_start_with(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}
/// Returns `true` if the given character can appear as a subsequent character of a label.
pub fn can_contain(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

impl<'a> TryParse<'a> for Label<'a> {
    /// Parse argument string as a [`Label`].
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
            0 // Avoid triggering error when "" doesn't parse as an integer
        } else {
            match Integer::try_parse_signed(offset_str)? {
                Some(offset) => offset.as_i16()?,
                None => return Err(error::Value::MalformedLabel {}),
            }
        };

        Ok(Some(Label { name, offset }))
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_parse() {
        macro_rules! expect_label {
            ( $input:expr, $($expected:tt)* ) => {{
                assert_eq!(
                    Label::try_parse($input).map_err(|_| ()),
                    $($expected)*
                );
            }};
        }

        expect_label!("", Ok(None));
        expect_label!("0x1283", Ok(None));
        expect_label!("!@*)#", Ok(None));
        expect_label!("0Foo", Ok(None));
        expect_label!("Foo!", Err(()));
        expect_label!("F", Ok(Some(Label::new("F", 0))));
        expect_label!("Foo", Ok(Some(Label::new("Foo", 0))));
        expect_label!("_Foo", Ok(Some(Label::new("_Foo", 0))));
        expect_label!("F_oo12", Ok(Some(Label::new("F_oo12", 0))));
        expect_label!("Foo12_", Ok(Some(Label::new("Foo12_", 0))));
        expect_label!("Foo+0", Ok(Some(Label::new("Foo", 0))));
        expect_label!("Foo-0", Ok(Some(Label::new("Foo", 0))));
        expect_label!("Foo+4", Ok(Some(Label::new("Foo", 4))));
        expect_label!("Foo-43", Ok(Some(Label::new("Foo", -43))));
        expect_label!("Foo+", Err(()));
        expect_label!("Foo-", Err(()));
        expect_label!("Foo", Ok(Some(Label::new("Foo", 0))));
        expect_label!("Foo+4", Ok(Some(Label::new("Foo", 4))));
        expect_label!("Foo+", Err(()));
        expect_label!("Foo-", Err(()));
        expect_label!("Foo+0x034", Ok(Some(Label::new("Foo", 0x34))));
        expect_label!("Foo-0o4", Ok(Some(Label::new("Foo", -4))));
        expect_label!("Foo-#24", Ok(Some(Label::new("Foo", -24))));
        expect_label!("Foo+#024", Ok(Some(Label::new("Foo", 24))));
    }

    #[test]
    fn byte_counted() {
        let mut chars = ByteCounted::from("abc🍋🍎f".chars().peekable());
        assert_eq!(chars.len(), 0);
        assert_eq!(chars.next(), Some('a'));
        assert_eq!(chars.len(), 1);
        assert_eq!(chars.next(), Some('b'));
        assert_eq!(chars.len(), 2);
        assert_eq!(chars.peek(), Some(&'c'));
        assert_eq!(chars.len(), 2);
        assert_eq!(chars.peek(), Some(&'c'));
        assert_eq!(chars.len(), 2);
        assert_eq!(chars.next(), Some('c'));
        assert_eq!(chars.len(), 3);
        assert_eq!(chars.next(), Some('🍋'));
        assert_eq!(chars.len(), 7);
        assert_eq!(chars.peek(), Some(&'🍎'));
        assert_eq!(chars.len(), 7);
        assert_eq!(chars.next(), Some('🍎'));
        assert_eq!(chars.len(), 11);
        assert_eq!(chars.next(), Some('f'));
        assert_eq!(chars.len(), 12);
        assert_eq!(chars.next(), None);
        assert_eq!(chars.len(), 12);
        assert_eq!(chars.next(), None);
        assert_eq!(chars.len(), 12);
    }
}
