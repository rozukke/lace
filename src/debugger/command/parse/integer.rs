use std::ops::Deref;

use super::{error, CharIter, TryParse};

/// Internal type of [`Integer`], before being converted to smaller type (`u16` or `i16`).
type IntegerValue = i32;

/// Wrapper type to implement [`TryParse`] for integer literal.
///
/// Internal type is larger than both `u16` and `i16`, to allow conversion to either type easily.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Integer {
    value: IntegerValue,
}

/// Value of sign character.
///
/// Do not default to [`Sign::Positive`] for missing/implicit sign character; Instead use
/// `Option<Sign>`
#[derive(Clone, Copy, Debug)]
enum Sign {
    Positive = 1,
    Negative = -1,
}

/// Radix (base) of integer, as determined by (optional) integer prefix:
#[derive(Clone, Copy, Debug)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

/// Helper struct for retaining syntax information when parsing integer prefix.
struct Prefix {
    /// Radix corresponding to prefix character.
    radix: Radix,
    /// Whether prefix character is preceeded by zeros.
    leading_zeros: bool,
    /// Whether prefix character is a symbol (i.e. "#").
    non_alpha: bool,
}

/// Helper struct similar to `Option<Prefix>` but also handles "0" case.
enum PrefixResult {
    /// Normal integer with (explicit or implicit) prefix.
    Integer(Prefix),
    /// Special case to handle "0".
    SingleZero,
    /// This token is not an integer, but not necessary invalid (yet).
    NonInteger,
}

impl<'a> TryParse<'a> for Integer {
    /// Parse argument string as an [`Integer`].
    ///
    /// Extremely liberal in accepted syntax.
    ///
    /// Accepts:
    ///  - Decimal (optional "#"), hex ("x"/"X"), octal ("o"/"O"), and binary ("b"/"B").
    ///  - Optional single zero before non-decimal radix prefix. Eg. "0x4".
    ///  - Leading zeros after prefix and sign. Eg. "0x0004", "#-03".
    ///  - Sign character before XOR after radix prefix. Eg. "-#2", "x+4".
    ///
    /// Returns `Ok(None)` (not an integer) for:
    ///  - Empty token.
    ///  - Non-decimal radix prefix, with no zero before it, and non-digits after it. Eg. "xLabel", "o".
    ///
    /// Returns `Err` (invalid integer and invalid token) for:
    ///  - Invalid digits for the given radix.
    ///  - Decimal radix prefix "#" with zeros before it. Eg. "0#2".
    ///  - Decimal radix prefix "#" with no digits after it. Eg. "#".
    ///  - Multiple sign characters (before or after prefix).
    ///  - Multiple zeros before radix prefix. Eg. "00x4".
    ///  - Absolute value out of bounds for `i32`. (Does *NOT* check if integer fits in specific bit size).
    fn try_parse(string: &str) -> Result<Option<Self>, error::Value> {
        parse_integer(string, false)
    }
}

impl Integer {
    /// Parse argument string as an [`Integer`], with mandatory pre-prefix sign character.
    ///
    /// See [`Integer::try_parse`] implementation for accepted syntax.
    pub fn try_parse_signed(string: &str) -> Result<Option<Self>, error::Value> {
        parse_integer(string, true)
    }

    /// Try to convert into `i16`.
    pub fn as_i16(self) -> Result<i16, error::Value> {
        (*self)
            .try_into()
            .map_err(|_| error::Value::IntegerTooLarge {
                max: i16::MAX as u16,
            })
    }

    /// Try to convert into `u16`.
    pub fn as_u16(self) -> Result<u16, error::Value> {
        (*self)
            .try_into()
            .map_err(|_| error::Value::IntegerTooLarge { max: u16::MAX })
    }

    /// Try to convert into `u16`, casting negative values.
    pub fn as_u16_cast(self) -> Result<u16, error::Value> {
        if *self < 0 {
            Ok(self.as_i16()? as u16)
        } else {
            self.as_u16()
        }
    }
}

impl<T> From<T> for Integer
where
    T: Into<IntegerValue>,
{
    fn from(value: T) -> Self {
        Self {
            value: value.into(),
        }
    }
}
impl Deref for Integer {
    type Target = IntegerValue;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Radix {
    /// Parse a single digit in a given radix.
    pub fn parse_digit(&self, ch: char) -> Option<u8> {
        Some(match self {
            Self::Binary => match ch {
                '0' => 0,
                '1' => 1,
                _ => return None,
            },
            Self::Octal => match ch {
                '0'..='7' => ch as u8 - b'0',
                _ => return None,
            },
            Self::Decimal => match ch {
                '0'..='9' => ch as u8 - b'0',
                _ => return None,
            },
            Self::Hex => match ch {
                '0'..='9' => ch as u8 - b'0',
                'a'..='f' => ch as u8 - b'a' + 10,
                'A'..='F' => ch as u8 - b'A' + 10,
                _ => return None,
            },
        })
    }
}

/// See [`Integer::try_parse`] implementation for accepted syntax.
fn parse_integer(string: &str, require_sign: bool) -> Result<Option<Integer>, error::Value> {
    // Useful for parsing label/pc offset
    if string.is_empty() {
        return Ok(None);
    }

    let mut chars: CharIter = string.chars().peekable();

    // Take sign BEFORE prefix
    let first_sign = take_sign(&mut chars);
    // Missing sign character before prefix
    if require_sign && first_sign.is_none() {
        return Err(error::Value::MalformedInteger {});
    }

    let prefix = match take_prefix(&mut chars)? {
        PrefixResult::Integer(prefix) => prefix,
        // Bypass normal parsing
        // The string must be "0" so no concerns about trailing characters
        PrefixResult::SingleZero => return Ok(Some(0.into())),

        PrefixResult::NonInteger => {
            // Sign was already given, so it must be an invalid token
            if first_sign.is_some() {
                return Err(error::Value::MalformedInteger {});
            }
            return Ok(None);
        }
    };

    // Take sign AFTER prefix
    let second_sign = take_sign(&mut chars);

    // Reconcile multiple sign characters
    let sign = match (first_sign, second_sign) {
        // Take either value
        (None, None) => None,
        (Some(sign), None) | (None, Some(sign)) => Some(sign),
        // Disallow multiple sign characters: "-x-...", "++...", etc
        (Some(_), Some(_)) => return Err(error::Value::MalformedInteger {}),
    };

    // Check next character is digit
    // Character must be checked against radix here to prevent valid non-integer tokens returning `Err`
    if chars
        .peek()
        .is_none_or(|ch| prefix.radix.parse_digit(*ch).is_none())
    {
        // Sign, pre-prefix zeros, or non-alpha prefix ("#") were given, so it must be an invalid integer token
        if sign.is_some() || prefix.leading_zeros || prefix.non_alpha {
            return Err(error::Value::MalformedInteger {});
        }
        return Ok(None);
    };

    // Take digits until non-digit character
    // Note that this loop handles post-prefix leading zeros like any other digit
    let mut integer: IntegerValue = 0;
    for ch in chars.by_ref() {
        // Invalid digit will always return `Err`
        // Valid non-integer tokens should trigger early return before this loop
        let Some(digit) = prefix.radix.parse_digit(ch) else {
            return Err(error::Value::MalformedInteger {});
        };

        // Re-checked later on convert to smaller int types
        if integer > IntegerValue::MAX / prefix.radix as IntegerValue {
            return Err(error::Value::IntegerTooLarge {
                max: i16::MAX as u16,
            });
        }

        integer *= prefix.radix as IntegerValue;
        integer += digit as IntegerValue;
    }

    assert!(
        chars.next().is_none(),
        "should have looped until end of argument, or early-returned `Err`",
    );

    // This should not overflow because any positive i32 can be negated as a valid i32
    if let Some(sign) = sign {
        integer *= sign as IntegerValue;
    }

    Ok(Some(integer.into()))
}

/// If next character is `'` or `+`, then consume character and return corresponding [`Sign`].
fn take_sign(chars: &mut CharIter) -> Option<Sign> {
    let sign = match chars.peek() {
        Some('+') => Sign::Positive,
        Some('-') => Sign::Negative,
        _ => return None,
    };
    chars.next();
    Some(sign)
}

/// If the next characters are a valid radix, then consume characters and return information about
/// prefix syntax.
///
/// Only the prefix is checked, so [`PrefixResult::Integer`] is returned for any label argument
/// which begins with a valid integer prefix (eg. "xLabel"). This must be handled by the caller.
///
/// A single-character "0" argument is a special case which is returned as
/// [`PrefixResult::SingleZero`]. This must be handled by the caller.
///
/// If the argument begins with a decimal digit (`[0-9]`), it is treated similar to if it began
/// with the decimal prefix "#".
///
/// Prefix character, and any single pre-prefix leading zero, are consumed from the iterator. No
/// other characters are consumed, even for arguments without a prefix character.
fn take_prefix(chars: &mut CharIter) -> Result<PrefixResult, error::Value> {
    // Only take ONE leading zero here
    // Disallow "00x..." etc.
    let leading_zeros = match chars.peek() {
        Some('0') => {
            chars.next();
            true
        }
        _ => false,
    };

    // Take optional prefix
    let mut consume_char = true;
    let (radix, non_alpha) = match chars.peek() {
        Some('b' | 'B') => (Radix::Binary, false),
        Some('o' | 'O') => (Radix::Octal, false),
        Some('x' | 'X') => (Radix::Hex, false),

        Some('#') => {
            // Disallow "0#..."
            if leading_zeros {
                return Err(error::Value::MalformedInteger {});
            }
            (Radix::Decimal, true)
        }

        // No prefix
        Some('0'..='9') => {
            consume_char = false; // Leave initial digit, to be parsed by caller
            (Radix::Decimal, false)
        }

        // Disallow "0-..." and "0+..."
        // Disallow "--...", "-+...", etc
        // Any legal pre-prefix sign character would have already been consumed
        Some('-' | '+') => {
            return Err(error::Value::MalformedInteger {});
        }

        // Special case for "0"
        // Only a single "leading" zero (no sign or prefix)
        None if leading_zeros => {
            return Ok(PrefixResult::SingleZero);
        }

        _ => {
            return Ok(PrefixResult::NonInteger);
        }
    };

    if consume_char {
        chars.next();
    }

    Ok(PrefixResult::Integer(Prefix {
        radix,
        leading_zeros,
        non_alpha,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        macro_rules! expect_integer {
            ( $method:tt, $input:expr, $($expected:tt)* ) => {{
                assert!(matches!(
                    expect_integer!(@method $method)($input)
                        .map(|opt| opt.map(|integer| *integer)),
                    $($expected)*
                ));
            }};
            (@method false) => {{ Integer::try_parse }};
            (@method true)  => {{ Integer::try_parse_signed }};
        }

        // These tests cover all edge cases which I can think of

        // Invalid or non-integers
        expect_integer!(false, "", Ok(None)); // Non-integer
        expect_integer!(false, "a", Ok(None));
        expect_integer!(false, "z", Ok(None));
        expect_integer!(false, "&", Ok(None));
        expect_integer!(false, ",", Ok(None));
        expect_integer!(false, "b2", Ok(None));
        expect_integer!(false, "o8", Ok(None));
        expect_integer!(false, "xg", Ok(None));
        expect_integer!(false, "b", Ok(None));
        expect_integer!(false, "o", Ok(None));
        expect_integer!(false, "x", Ok(None));
        expect_integer!(false, "-", Err(_)); // Invalid integers
        expect_integer!(false, "+", Err(_));
        expect_integer!(false, "#", Err(_));
        expect_integer!(false, "#-", Err(_));
        expect_integer!(false, "-#", Err(_));
        expect_integer!(false, "-#-", Err(_));
        expect_integer!(false, "-#-24", Err(_));
        expect_integer!(false, "0#0", Err(_));
        expect_integer!(false, "0#24", Err(_));
        expect_integer!(false, "-0#24", Err(_));
        expect_integer!(false, "0#-24", Err(_));
        expect_integer!(false, "-0#-24", Err(_));
        expect_integer!(false, "x-", Err(_));
        expect_integer!(false, "-x", Err(_));
        expect_integer!(false, "-x-", Err(_));
        expect_integer!(false, "-x-24", Err(_));
        expect_integer!(false, "0x", Err(_));
        expect_integer!(false, "0x-", Err(_));
        expect_integer!(false, "-0x", Err(_));
        expect_integer!(false, "-0x-", Err(_));
        expect_integer!(false, "-0x-24", Err(_));
        expect_integer!(false, "0-x24", Err(_));
        expect_integer!(false, "00x4", Err(_));
        expect_integer!(false, "##", Err(_)); // Invalid digit for decimal base
        expect_integer!(false, "-##", Err(_));
        expect_integer!(false, "#b", Err(_));
        expect_integer!(false, "#-b", Err(_));
        expect_integer!(false, "-#b", Err(_));
        expect_integer!(false, "0b2", Err(_)); // Invalid digit for base
        expect_integer!(false, "0o8", Err(_));
        expect_integer!(false, "0xg", Err(_));
        expect_integer!(false, "-b2", Err(_));
        expect_integer!(false, "-o8", Err(_));
        expect_integer!(false, "-xg", Err(_));
        expect_integer!(false, "b-2", Err(_));
        expect_integer!(false, "o-8", Err(_));
        expect_integer!(false, "x-g", Err(_));
        expect_integer!(false, "--4", Err(_)); // Multiple sign characters
        expect_integer!(false, "-+4", Err(_));
        expect_integer!(false, "++4", Err(_));
        expect_integer!(false, "+-4", Err(_));
        expect_integer!(false, "#--4", Err(_));
        expect_integer!(false, "#-+4", Err(_));
        expect_integer!(false, "#++4", Err(_));
        expect_integer!(false, "#+-4", Err(_));
        expect_integer!(false, "-#-4", Err(_));
        expect_integer!(false, "-#+4", Err(_));
        expect_integer!(false, "+#+4", Err(_));
        expect_integer!(false, "+#-4", Err(_));
        expect_integer!(false, "--#4", Err(_));
        expect_integer!(false, "-+#4", Err(_));
        expect_integer!(false, "++#4", Err(_));
        expect_integer!(false, "+-#4", Err(_));
        expect_integer!(true, "--4", Err(_));
        expect_integer!(true, "#--4", Err(_));
        expect_integer!(true, "+#-4", Err(_));
        expect_integer!(true, "+-#4", Err(_));
        expect_integer!(true, "#4", Err(_)); // Missing sign character
        expect_integer!(true, "x4", Err(_));
        // Simple bounds check (it is not supposed to be super accurate)
        expect_integer!(false, "x80000000", Err(_));
        expect_integer!(false, "x7fffffff", Ok(Some(0x7fffffff)));
        expect_integer!(false, "x-7fffffff", Ok(Some(-0x7fffffff)));
        expect_integer!(false, "x-80000000", Err(_));
        // Decimal
        expect_integer!(false, "0", Ok(Some(0)));
        expect_integer!(false, "00", Ok(Some(0)));
        expect_integer!(false, "#0", Ok(Some(0)));
        expect_integer!(false, "#00", Ok(Some(0)));
        expect_integer!(false, "-#0", Ok(Some(0)));
        expect_integer!(false, "+#0", Ok(Some(0)));
        expect_integer!(false, "-#00", Ok(Some(0)));
        expect_integer!(false, "#-0", Ok(Some(0)));
        expect_integer!(false, "#+0", Ok(Some(0)));
        expect_integer!(false, "#-00", Ok(Some(0)));
        expect_integer!(false, "4", Ok(Some(4)));
        expect_integer!(false, "+4", Ok(Some(4)));
        expect_integer!(false, "4284", Ok(Some(4284)));
        expect_integer!(false, "004284", Ok(Some(4284)));
        expect_integer!(false, "#4", Ok(Some(4)));
        expect_integer!(false, "#4284", Ok(Some(4284)));
        expect_integer!(false, "#004284", Ok(Some(4284)));
        expect_integer!(false, "-4", Ok(Some(-4)));
        expect_integer!(false, "+4", Ok(Some(4)));
        expect_integer!(false, "-4284", Ok(Some(-4284)));
        expect_integer!(false, "-004284", Ok(Some(-4284)));
        expect_integer!(false, "-#4", Ok(Some(-4)));
        expect_integer!(false, "+#4", Ok(Some(4)));
        expect_integer!(false, "-#4284", Ok(Some(-4284)));
        expect_integer!(false, "-#004284", Ok(Some(-4284)));
        expect_integer!(false, "#-4", Ok(Some(-4)));
        expect_integer!(false, "#+4", Ok(Some(4)));
        expect_integer!(false, "#-4284", Ok(Some(-4284)));
        expect_integer!(false, "#-004284", Ok(Some(-4284)));
        expect_integer!(true, "-4", Ok(Some(-4)));
        expect_integer!(true, "+4", Ok(Some(4)));
        expect_integer!(true, "-4284", Ok(Some(-4284)));
        expect_integer!(true, "-004284", Ok(Some(-4284)));
        expect_integer!(true, "-#4", Ok(Some(-4)));
        expect_integer!(true, "+#4", Ok(Some(4)));
        expect_integer!(true, "-#4284", Ok(Some(-4284)));
        expect_integer!(true, "-#004284", Ok(Some(-4284)));
        expect_integer!(true, "#-4", Err(_));
        expect_integer!(true, "#+4", Err(_));
        expect_integer!(true, "#-4284", Err(_));
        expect_integer!(true, "#-004284", Err(_));
        expect_integer!(true, "4", Err(_));
        expect_integer!(true, "4284", Err(_));
        expect_integer!(true, "004284", Err(_));
        expect_integer!(true, "#4", Err(_));
        expect_integer!(true, "#4284", Err(_));
        expect_integer!(true, "#004284", Err(_));
        expect_integer!(true, "#4", Err(_));
        // Hex
        expect_integer!(false, "x0", Ok(Some(0x0)));
        expect_integer!(false, "x00", Ok(Some(0x0)));
        expect_integer!(false, "0x0", Ok(Some(0x0)));
        expect_integer!(false, "0x00", Ok(Some(0x0)));
        expect_integer!(false, "-x0", Ok(Some(0x0)));
        expect_integer!(false, "+x0", Ok(Some(0x0)));
        expect_integer!(false, "-x00", Ok(Some(0x0)));
        expect_integer!(false, "0x-0", Ok(Some(0x0)));
        expect_integer!(false, "0x-00", Ok(Some(0x0)));
        expect_integer!(false, "-0x0", Ok(Some(0x0)));
        expect_integer!(false, "-0x00", Ok(Some(0x0)));
        expect_integer!(false, "x4", Ok(Some(0x4)));
        expect_integer!(false, "x004", Ok(Some(0x4)));
        expect_integer!(false, "x429", Ok(Some(0x429)));
        expect_integer!(false, "0x4", Ok(Some(0x4)));
        expect_integer!(false, "0x004", Ok(Some(0x4)));
        expect_integer!(false, "0x429", Ok(Some(0x429)));
        expect_integer!(false, "-x4", Ok(Some(-0x4)));
        expect_integer!(false, "+x4", Ok(Some(0x4)));
        expect_integer!(false, "-x004", Ok(Some(-0x4)));
        expect_integer!(false, "-x429", Ok(Some(-0x429)));
        expect_integer!(false, "-0x4", Ok(Some(-0x4)));
        expect_integer!(false, "+0x4", Ok(Some(0x4)));
        expect_integer!(false, "-0x004", Ok(Some(-0x4)));
        expect_integer!(false, "-0x429", Ok(Some(-0x429)));
        expect_integer!(false, "x-4", Ok(Some(-0x4)));
        expect_integer!(false, "x-004", Ok(Some(-0x4)));
        expect_integer!(false, "x+004", Ok(Some(0x4)));
        expect_integer!(false, "x-429", Ok(Some(-0x429)));
        expect_integer!(false, "-0x4", Ok(Some(-0x4)));
        expect_integer!(false, "-0x004", Ok(Some(-0x4)));
        expect_integer!(false, "-0x429", Ok(Some(-0x429)));
        expect_integer!(false, "+0x429", Ok(Some(0x429)));
        expect_integer!(true, "-x4", Ok(Some(-0x4)));
        expect_integer!(true, "+x4", Ok(Some(0x4)));
        expect_integer!(true, "-x004", Ok(Some(-0x4)));
        expect_integer!(true, "-x429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "+0x4", Ok(Some(0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(true, "-0x4", Ok(Some(-0x4)));
        expect_integer!(true, "-0x004", Ok(Some(-0x4)));
        expect_integer!(true, "-0x429", Ok(Some(-0x429)));
        expect_integer!(true, "+0x429", Ok(Some(0x429)));
        expect_integer!(true, "x-4", Err(_));
        expect_integer!(true, "x-004", Err(_));
        expect_integer!(true, "x+004", Err(_));
        expect_integer!(true, "x-429", Err(_));
        expect_integer!(true, "x4", Err(_));
        expect_integer!(true, "x004", Err(_));
        expect_integer!(true, "x429", Err(_));
        expect_integer!(true, "0x4", Err(_));
        expect_integer!(true, "0x004", Err(_));
        expect_integer!(true, "0x429", Err(_));
        expect_integer!(true, "x4", Err(_));
        expect_integer!(true, "x004", Err(_));
        expect_integer!(true, "x429", Err(_));
        expect_integer!(true, "0x4", Err(_));
        expect_integer!(true, "0x004", Err(_));
        expect_integer!(true, "0x429", Err(_));
        expect_integer!(true, "0x429", Err(_));
        // Octal (0o427==0x117)
        expect_integer!(false, "o0", Ok(Some(0x0)));
        expect_integer!(false, "o00", Ok(Some(0x0)));
        expect_integer!(false, "0o0", Ok(Some(0x0)));
        expect_integer!(false, "0o00", Ok(Some(0x0)));
        expect_integer!(false, "-o0", Ok(Some(0x0)));
        expect_integer!(false, "-o00", Ok(Some(0x0)));
        expect_integer!(false, "o-0", Ok(Some(0x0)));
        expect_integer!(false, "o-00", Ok(Some(0x0)));
        expect_integer!(false, "-0o0", Ok(Some(0x0)));
        expect_integer!(false, "-0o00", Ok(Some(0x0)));
        expect_integer!(false, "0o-0", Ok(Some(0x0)));
        expect_integer!(false, "0o-00", Ok(Some(0x0)));
        expect_integer!(false, "o4", Ok(Some(0x4)));
        expect_integer!(false, "o004", Ok(Some(0x4)));
        expect_integer!(false, "o427", Ok(Some(0x117)));
        expect_integer!(false, "0o4", Ok(Some(0x4)));
        expect_integer!(false, "0o004", Ok(Some(0x4)));
        expect_integer!(false, "0o427", Ok(Some(0x117)));
        expect_integer!(false, "-o4", Ok(Some(-0x4)));
        expect_integer!(false, "-o004", Ok(Some(-0x4)));
        expect_integer!(false, "-o427", Ok(Some(-0x117)));
        expect_integer!(false, "-0o4", Ok(Some(-0x4)));
        expect_integer!(false, "-0o004", Ok(Some(-0x4)));
        expect_integer!(false, "-0o427", Ok(Some(-0x117)));
        expect_integer!(false, "o-4", Ok(Some(-0x4)));
        expect_integer!(false, "o-004", Ok(Some(-0x4)));
        expect_integer!(false, "o-427", Ok(Some(-0x117)));
        expect_integer!(false, "0o-4", Ok(Some(-0x4)));
        expect_integer!(false, "0o-004", Ok(Some(-0x4)));
        expect_integer!(false, "0o-427", Ok(Some(-0x117)));
        // Binary
        expect_integer!(false, "b0", Ok(Some(0b0)));
        expect_integer!(false, "b00", Ok(Some(0b0)));
        expect_integer!(false, "0b0", Ok(Some(0b0)));
        expect_integer!(false, "0b00", Ok(Some(0b0)));
        expect_integer!(false, "-b0", Ok(Some(0b0)));
        expect_integer!(false, "-b00", Ok(Some(0b0)));
        expect_integer!(false, "b-0", Ok(Some(0b0)));
        expect_integer!(false, "b-00", Ok(Some(0b0)));
        expect_integer!(false, "-0b0", Ok(Some(0b0)));
        expect_integer!(false, "-0b00", Ok(Some(0b0)));
        expect_integer!(false, "0b-0", Ok(Some(0b0)));
        expect_integer!(false, "0b-00", Ok(Some(0b0)));
        expect_integer!(false, "b1", Ok(Some(0b1)));
        expect_integer!(false, "b101", Ok(Some(0b101)));
        expect_integer!(false, "b00101", Ok(Some(0b101)));
        expect_integer!(false, "0b1", Ok(Some(0b1)));
        expect_integer!(false, "0b101", Ok(Some(0b101)));
        expect_integer!(false, "0b00101", Ok(Some(0b101)));
        expect_integer!(false, "-b1", Ok(Some(-0b1)));
        expect_integer!(false, "-b101", Ok(Some(-0b101)));
        expect_integer!(false, "-b00101", Ok(Some(-0b101)));
        expect_integer!(false, "b-1", Ok(Some(-0b1)));
        expect_integer!(false, "b-101", Ok(Some(-0b101)));
        expect_integer!(false, "b-00101", Ok(Some(-0b101)));
        expect_integer!(false, "-0b1", Ok(Some(-0b1)));
        expect_integer!(false, "-0b101", Ok(Some(-0b101)));
        expect_integer!(false, "-0b00101", Ok(Some(-0b101)));
        expect_integer!(false, "0b-1", Ok(Some(-0b1)));
        expect_integer!(false, "0b-101", Ok(Some(-0b101)));
        expect_integer!(false, "0b-00101", Ok(Some(-0b101)));
    }
}
