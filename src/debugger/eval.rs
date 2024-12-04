use miette::Result;

use crate::{
    air::{AirStmt, AsmLine},
    runtime::RunState,
    AsmParser,
};

pub fn run(state: &mut RunState, line: String) -> Result<()> {
    let stmt = parse(line)?;
    let line = AsmLine::new(0, stmt);
    let instr = line.emit()?;
    execute(state, instr);
    Ok(())
}

fn parse(line: String) -> Result<AirStmt> {
    // Required to make temporarily 'static
    let line_ptr = Box::into_raw(line.into_boxed_str());
    let line = unsafe { &*line_ptr };

    // Do not return early or memory will not be freed
    let result = AsmParser::new_simple(line).and_then(|mut parser| parser.parse_simple());

    unsafe { drop(Box::from_raw(line_ptr)) };

    result
}

fn execute(state: &mut RunState, instr: u16) {
    let opcode = (instr >> 12) as usize;
    RunState::OP_TABLE[opcode](state, instr);
}
