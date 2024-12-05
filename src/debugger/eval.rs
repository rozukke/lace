use miette::Result;

use crate::{
    air::{AirStmt, AsmLine},
    runtime::RunState,
    AsmParser,
};

pub fn eval(state: &mut RunState, line: String) {
    // Required to make temporarily 'static
    let line_ptr = Box::into_raw(line.into_boxed_str());
    let line = unsafe { &*line_ptr };

    // Do not return early, to ensure string is freed
    if let Err(err) = run(state, line) {
        eprintln!("{:?}", err);
    }

    unsafe { drop(Box::from_raw(line_ptr)) };
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
