use std::io::{self, Read as _};

use super::{Read, INITIAL_BUFFER_CAPACITY};

/// Stdin which is not attached to a terminal, i.e. piped.
#[derive(Debug)]
pub struct Stdin {
    stdin: io::Stdin,
    /// Command must be stored somewhere to be referenced.
    buffer: String,
}

impl Stdin {
    pub fn from(stdin: io::Stdin) -> Self {
        Self {
            stdin,
            buffer: String::with_capacity(INITIAL_BUFFER_CAPACITY),
        }
    }

    /// `None` indicates EOF.
    fn read_char(&mut self) -> Option<char> {
        read_char_from_bytes(|| self.read_byte()).expect("uh oh")
    }

    /// `None` indicates EOF.
    fn read_byte(&mut self) -> Option<u8> {
        let mut buf = [0; 1];
        let bytes_read = self
            .stdin
            .read(&mut buf)
            .expect("failed to read character from stdin");
        if bytes_read == 0 {
            return None;
        }
        Some(buf[0])
    }
}

// TODO(feat): Use proper errors
fn read_char_from_bytes<F>(mut next_byte: F) -> Result<Option<char>, ()>
where
    F: FnMut() -> Option<u8>,
{
    // TODO(refactor): Make this nicer
    let Some(byte) = next_byte() else {
        return Ok(None);
    };

    let mut bytes = [byte, 0, 0, 0];

    let utf8_position = Utf8Position::from(byte);
    let Some(utf8_len) = utf8_position.len() else {
        return Err(());
    };

    for i in 1..utf8_len {
        let Some(byte) = next_byte() else {
            return Err(());
        };
        if !Utf8Position::from(byte).is_continuation() {
            return Err(());
        }
        bytes[i] = byte;
    }

    let string = std::str::from_utf8(&bytes[0..utf8_len]).map_err(|_| ())?;
    let mut chars = string.chars();
    let ch = chars.next().ok_or(())?;
    if chars.next().is_some() {
        return Err(());
    }
    Ok(Some(ch))
}

/// Position of byte inside UTF-8 character
enum Utf8Position {
    /// First byte of 4-byte character
    Begin4,
    /// First byte of 3-byte character
    Begin3,
    /// First byte of 2-byte character
    Begin2,
    /// Only byte of 1-byte character
    Begin1,
    /// Continuation of multi-byte character
    Continuation,
}

impl Utf8Position {
    pub fn from(byte: u8) -> Self {
        // TODO(refactor): Make this nicer
        const MASK_4: u8 = 0b1111_0000;
        const MASK_3: u8 = 0b1110_0000;
        const MASK_2: u8 = 0b1100_0000;
        const MASK_CONT: u8 = 0b1000_0000;
        if byte & MASK_4 == MASK_4 {
            return Utf8Position::Begin4;
        }
        if byte & MASK_3 == MASK_3 {
            return Utf8Position::Begin3;
        }
        if byte & MASK_2 == MASK_2 {
            return Utf8Position::Begin2;
        }
        if byte & MASK_CONT == MASK_CONT {
            return Utf8Position::Continuation;
        }
        Utf8Position::Begin1
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            Self::Begin4 => Some(4),
            Self::Begin3 => Some(3),
            Self::Begin2 => Some(2),
            Self::Begin1 => Some(1),
            Self::Continuation => None,
        }
    }

    pub fn is_continuation(&self) -> bool {
        matches!(self, Self::Continuation)
    }
}

impl Read for Stdin {
    fn read(&mut self) -> Option<&str> {
        self.buffer.clear();

        // Take characters until delimiter
        loop {
            let Some(ch) = self.read_char() else {
                if self.buffer.is_empty() {
                    return None; // First character is EOF
                }
                break;
            };
            if ch == '\n' || ch == ';' {
                break;
            }
            self.buffer.push(ch);
        }

        Some(&self.buffer)
    }
}
