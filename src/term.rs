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

/// Read terminal events until key event is read as a valid [`Key`].
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
