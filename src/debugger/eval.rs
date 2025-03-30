use miette::Result;

use crate::air::{AirStmt, AsmLine};
use crate::runtime::RunState;
use crate::symbol::Span;
use crate::{dprintln, AsmParser};

pub fn eval(state: &mut RunState, line: &str) {
    // Required to make temporarily 'static
    // SAFETY: `line` is not used after being dropped (i.e. not returned or used in a greater
    // scope)
    let line_static = unsafe { &*(line as *const str) };
    if let Err(err) = eval_inner(state, line_static) {
        eprintln!("{:?}", err);
    }
}

/// Wrapper to group errors into one location
fn eval_inner(state: &mut RunState, line: &'static str) -> Result<()> {
    // Parse
    let stmt = AsmParser::new_simple(line)?.parse_simple()?;

    match stmt {
        // Don't allow any branch instructions
        // Since CC is set to 0b000 at start, this could lead to confusion when `BR` instructions
        // are not executed
        AirStmt::Branch { .. } => {
            dprintln!(
                Alternate,
                Error,
                "DisallowedInstruction::Branch",
                ["Simulating `BR*` instructions is not permitted."],
            );
            dprintln!(Sometimes, Error, "Consider using `jump` command instead.");
            return Ok(());
        }

        // Don't allow `RTI` (interrupt) instruction
        // Since it can only be used in supervisor mode, and it is unimplemented regardless
        AirStmt::Interrupt => {
            dprintln!(
                Alternate,
                Error,
                "DisallowedInstruction::Interrupt",
                ["Simulating `RTI` instruction is not permitted."],
            );
            dprintln!(Sometimes, Error, "Don't even think about it.");
            return Ok(());
        }

        // Don't allow `HALT` instruction
        // Since `HALT` is treated specially by debugger
        AirStmt::Trap { trap_vect: 0x25 } => {
            dprintln!(
                Alternate,
                Error,
                "DisallowedInstruction::Halt",
                ["Simulating `HALT` trap instruction is not permitted."],
            );
            dprintln!(Sometimes, Error, "Consider using `exit` command instead.");
            return Ok(());
        }

        // Don't allow unknown/invalid trap instructions
        // To prevent exception and program exit
        // WARNING: If custom traps are implemented, this condition should be changed!!
        // If this branch is taken when using a trap mnemonic (eg. `eval reg`), then this is a
        // mistake!
        AirStmt::Trap { trap_vect } if !(0x20..=0x27).contains(&trap_vect) => {
            dprintln!(
                Alternate,
                Error,
                "DisallowedInstruction::UnknownTrap",
                ["Simulating invalid or unknown trap instructions is not permitted."],
            );
            dprintln!(Sometimes, Error, "What are you even trying to do?");
            return Ok(());
        }

        AirStmt::RawWord { .. } => unreachable!("tried to simulate raw word"),

        _ => (),
    }

    // Check labels
    let mut asm = AsmLine::new(0, stmt, Span::dummy());
    asm.backpatch()?;

    // Compile and execute
    let instr = asm.emit()?;
    state.execute(instr);

    Ok(())
}
