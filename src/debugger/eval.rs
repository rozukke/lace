use miette::Result;

use crate::{
    air::{AirStmt, AsmLine},
    runtime::RunState,
    AsmParser,
};

/// Get an unsafe `&'static str` from a `Box<str>`, for temporary use
struct StaticStr {
    ptr: *mut str,
}

impl StaticStr {
    pub unsafe fn as_str(&self) -> &'static str {
        &*self.ptr
    }
}

impl<T> From<T> for StaticStr
where
    T: Into<Box<str>>,
{
    fn from(string: T) -> Self {
        Self {
            ptr: Box::into_raw(string.into()),
        }
    }
}

impl Drop for StaticStr {
    fn drop(&mut self) {
        let boxed = unsafe { Box::from_raw(self.ptr) };
        drop(boxed);
    }
}

pub fn eval(state: &mut RunState, line: String) {
    // Required to make temporarily 'static
    // Automatically dropped at end of scope
    let line = StaticStr::from(line);

    // Note that error cannot be returned from this function, without the caller
    // being responsible for dropping `line`
    if let Err(err) = run(state, unsafe { line.as_str() }) {
        eprintln!("{:?}", err);
    }
}

// TODO(refactor): Rename `run`
fn run(state: &mut RunState, line: &'static str) -> Result<()> {
    let stmt = parse(line)?;
    let line = AsmLine::new(0, stmt);
    let instr = line.emit()?;
    execute(state, instr);
    Ok(())
}

fn parse(line: &'static str) -> Result<AirStmt> {
    let mut parser = AsmParser::new_simple(line)?;
    let line = parser.parse_simple()?;
    Ok(line)
}

fn execute(state: &mut RunState, instr: u16) {
    let opcode = (instr >> 12) as usize;
    RunState::OP_TABLE[opcode](state, instr);
}
