use std::ops::Deref;

use super::{error, CharIter};

type IntegerValue = i32;

#[derive(Debug)]
pub struct Integer {
    value: IntegerValue,
}

#[derive(Clone, Copy, Debug)]
enum Sign {
    Positive = 1,
    Negative = -1,
}

#[derive(Clone, Copy, Debug)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
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

impl Integer {
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

impl Deref for Integer {
    type Target = IntegerValue;

    fn deref(&self) -> &Self::Target {
        &self.value
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

/// Parse and consume the next integer argument.
///
/// Extremely liberal in accepted syntax.
///
/// Accepts:
///  - Decimal (optional "#"), hex ("x"/"X"), octal ("o"/"O"), and binary ("b"/"B").
///  - Optional single zero before non-decimal radix prefix. Eg. "0x4".
///  - Leading zeros after prefix and sign. Eg. "0x0004", "#-03".
///  - Sign character before xor after radix prefix. Eg. "-#2", "x+4".
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
///  - Missing sign character "-" or "+", if `require_sign == true`.
///  - Multiple zeros before radix prefix. Eg. "00x4".
///  - Absolute value out of bounds for `i32`. (Does *NOT* check if integer fits in specific bit size).
pub fn parse_integer(string: &str, require_sign: bool) -> Result<Option<Integer>, error::Value> {
    // Useful for parsing label/pc offset
    if string.is_empty() {
        return Ok(None);
    }

    let mut chars: CharIter = string.chars().peekable();

    // Take sign BEFORE prefix
    let first_sign = take_sign(&mut chars);

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
        (Some(sign), None) => Some(sign),
        (None, Some(sign)) => Some(sign),
        (None, None) => {
            if require_sign {
                return Err(error::Value::MalformedInteger {});
            }
            None
        }
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

    // TODO(fix): I think there is an edge case here for overflow
    if let Some(sign) = sign {
        integer *= sign as IntegerValue;
    }

    Ok(Some(integer.into()))
}

fn take_sign(chars: &mut CharIter) -> Option<Sign> {
    let sign = match chars.peek() {
        Some('+') => Sign::Positive,
        Some('-') => Sign::Negative,
        _ => return None,
    };
    chars.next();
    Some(sign)
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
