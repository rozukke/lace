use miette::Result;

use crate::air::AsmLine;
use crate::runtime::RunState;
use crate::AsmParser;

pub fn eval(state: &mut RunState, line: String) {
    // Required to make temporarily 'static
    // Automatically dropped at end of scope
    let line = StaticStr::from(line);

    // Note that error cannot be returned from this function, without the caller
    // being responsible for dropping `line`
    if let Err(err) = eval_inner(state, unsafe { line.as_str() }) {
        eprintln!("{:?}", err);
    }
}

// Wrapper to group errors into one location
fn eval_inner(state: &mut RunState, line: &'static str) -> Result<()> {
    // Parse
    let stmt = AsmParser::new_simple(line)?.parse_simple()?;
    // Emit
    let instr = AsmLine::new(0, stmt).emit()?;
    // Execute
    RunState::OP_TABLE[(instr >> 12) as usize](state, instr);
    Ok(())
}

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
