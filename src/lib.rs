// Parsing
mod parser;
pub use parser::AsmParser;
mod air;
pub use air::Air;

// Running
mod runtime;
pub use runtime::RunEnvironment;
#[macro_use]
mod debugger;
pub use debugger::DebuggerOptions;
mod mc;
mod output;

// Reset global state for watch
mod symbol;
pub use symbol::{reset_state, StaticSource};

mod error;
mod lexer;

pub mod env;

/// Amount of lines to show as context, each side of focus line (line containing span).
pub const DIAGNOSTIC_CONTEXT_LINES: usize = 8;
