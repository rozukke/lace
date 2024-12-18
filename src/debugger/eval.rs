use miette::Result;

use crate::air::{AirStmt, AsmLine};
use crate::runtime::RunState;
use crate::symbol::Span;
use crate::{dprintln, AsmParser};

// TODO(feat): Warn on `eval br* ...` and suggest `jump ...`

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
    // Don't allow *condition* branch instructions
    // Since CC is set to 0b000 at start, this could lead to confusion when `BR` instructions are
    // not executed
    match stmt {
        AirStmt::Branch { .. } => {
            dprintln!(
                Always,
                Error,
                "Evaluation of `BR*` instructions is not supported."
            );
            dprintln!(Always, Error, "Consider using `jump` command instead.");
            return Ok(());
        }
        _ => (),
    }
    // Check labels
    let mut asm = AsmLine::new(0, stmt, Span::dummy());
    asm.backpatch()?;
    // Emit
    let instr = asm.emit()?;
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
