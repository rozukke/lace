/// Represents a single "word" inside the parsed representation of source code.
pub struct Token {
    // Value contained inside the token
    pub val: String,
    // Line number inside the file
    pub line: u16,
    // Column number inside line
    pub col: u16,
}
