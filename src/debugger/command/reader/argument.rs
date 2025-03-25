use super::Read;

/// Command-line argument.
#[derive(Debug)]
pub struct Argument {
    buffer: String,
    /// Byte index.
    cursor: usize,
}

impl Argument {
    pub fn from(source: String) -> Self {
        Self {
            buffer: source,
            cursor: 0,
        }
    }
}

impl Read for Argument {
    fn read(&mut self) -> Option<&str> {
        // EOF
        if self.cursor >= self.buffer.len() {
            return None;
        }

        // Take characters until delimiter
        let start = self.cursor;
        let mut chars = self.buffer[self.cursor..].chars();
        while let Some(ch) = chars.next().filter(|ch| *ch != '\n' && *ch != ';') {
            self.cursor += ch.len_utf8();
        }

        let end = self.cursor;
        self.cursor += 1; // sizeof('\n' or ';')

        let command = self
            .buffer
            .get(start..end)
            .expect("calculated incorrect character indexes");
        Some(command)
    }
}
