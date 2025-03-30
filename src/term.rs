use std::cell::RefCell;

use crossterm::{
    event::{self, Event, KeyEvent},
    terminal,
};

/// Similar to [`crossterm::Event::KeyCode`] but only contains relevant information.
#[derive(Debug)]
pub enum Key {
    Enter,
    Backspace,
    Delete,
    Left,
    Right,
    Up,
    Down,
    CtrlLeft,
    CtrlRight,
    Char(char),
}

/// Must only be called if terminal is NOT in raw mode.
pub fn enable_raw_mode() {
    debug_assert!(
        !terminal::is_raw_mode_enabled().is_ok_and(|is| is),
        "terminal should not be in raw mode to enable raw mode",
    );
    terminal::enable_raw_mode().expect("failed to enable raw terminal");
}

/// Must only be called if terminal is in raw mode.
pub fn disable_raw_mode() {
    debug_assert!(
        terminal::is_raw_mode_enabled().is_ok_and(|is| is),
        "terminal should already be in raw mode to disable raw mode",
    );
    terminal::disable_raw_mode().expect("failed to disable raw terminal");
}

/// Read next key from interactive terminal.
///
/// Events are consumed until a key event is read as a valid [`Key`].
///
/// Caller must ensure terminal is in raw mode.
///
/// `Ctrl+C` will always return the terminal to normal state and exit.
pub fn read_key() -> Key {
    assert!(
        terminal::is_raw_mode_enabled().is_ok_and(|is| is),
        "terminal must be in raw mode to read key",
    );
    let key = loop {
        let event = event::read().expect("failed to read terminal event");
        if let Ok(key) = event.try_into() {
            break key;
        }
    };
    key
}

/// Read one byte from interactive terminal.
///
/// Inputs are read as [`KeyEvent`]s, which read multi-byte characters as `char`. Therefore,
/// any such characters are encoded in 1-4 bytes as UTF-8, and buffered for the next call.
///
/// Caller must ensure terminal is NOT in raw mode.
pub fn read_byte() -> Option<u8> {
    // Counter > 0: bytes are still 'buffered'
    if with_counter(|counter| {
        if *counter > 0 {
            *counter -= 1;
            return true;
        }
        false
    }) {
        return None;
    };

    let ch = read_char();

    // Convert `char` to iterator of bytes, which skips any zero-bytes
    let mut bytes = [0u8; 4];
    ch.encode_utf8(&mut bytes);
    let mut bytes = bytes.into_iter().filter(|byte| *byte != 0);

    let Some(first) = bytes.next() else {
        return Some(0); // All zero-bytes (length == 0): ASCII NUL
    };

    let count = bytes.count(); // = length - 1
    if count == 0 {
        return Some(first); // Single byte: Should be ASCII
    }

    // Multi-byte (length > 1): 'buffer' rest of characters (1-3) for next call
    with_counter(|counter| {
        *counter = count as u8;
    });
    None
}

thread_local! {
    /// Must only be used inside `read_byte`.
    static BUFFERED_BYTE_COUNT: RefCell<u8> = const { RefCell::new(0) };
}
fn with_counter<F, R>(func: F) -> R
where
    F: FnOnce(&mut u8) -> R,
{
    BUFFERED_BYTE_COUNT.with(|counter| func(&mut *counter.borrow_mut()))
}

/// Read single character from interactive terminal.
///
/// Loops until [`Key::Char`] or [`Key::Enter`] are read.
///
/// Caller must ensure terminal is NOT in raw mode.
fn read_char() -> char {
    enable_raw_mode();
    let ch = loop {
        match read_key() {
            Key::Char(ch) => break ch,
            Key::Enter => break '\n',
            _ => continue,
        };
    };
    disable_raw_mode();
    ch
}

impl TryFrom<Event> for Key {
    type Error = ();
    fn try_from(event: Event) -> Result<Self, Self::Error> {
        if let Event::Key(event) = event {
            if let Ok(key) = event.try_into() {
                return Ok(key);
            }
        }
        Err(())
    }
}

impl TryFrom<KeyEvent> for Key {
    type Error = ();
    fn try_from(event: KeyEvent) -> Result<Self, Self::Error> {
        use event::{KeyCode, KeyEventKind, KeyModifiers as Mod};

        if matches!(event.kind, KeyEventKind::Release) {
            return Err(());
        }

        let key = match (event.modifiers, event.code) {
            // Ctrl+C
            (Mod::CONTROL, KeyCode::Char('c')) => {
                disable_raw_mode(); // Generic cleanup
                println!();
                std::process::exit(0);
            }

            // Backspace, Delete, Enter
            (_, KeyCode::Backspace) => Key::Backspace,
            (_, KeyCode::Delete) => Key::Delete,
            (_, KeyCode::Enter) | (_, KeyCode::Char('\n')) => Key::Enter,

            // Arrow keys
            (Mod::NONE, KeyCode::Left) => Key::Left,
            (Mod::NONE, KeyCode::Right) => Key::Right,
            (Mod::NONE, KeyCode::Up) => Key::Up,
            (Mod::NONE, KeyCode::Down) => Key::Down,

            // Ctrl + Arrow keys
            (Mod::CONTROL, KeyCode::Left) => Key::CtrlLeft,
            (Mod::CONTROL, KeyCode::Right) => Key::CtrlRight,

            // Normal character
            (Mod::NONE | Mod::SHIFT, KeyCode::Char(ch)) => Key::Char(ch),

            _ => return Err(()),
        };

        Ok(key)
    }
}
