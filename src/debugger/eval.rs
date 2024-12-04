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
    // TODO(fix): This is a TERRIBLE solution. Ideally cursor doesn't take &'static str ??
    let line = Box::leak(line.into_boxed_str());

    let mut parser = AsmParser::new_simple(line)?;
    let stmt = parser.parse_simple()?;

    Ok(stmt)
}

fn execute(state: &mut RunState, instr: u16) {
    let opcode = (instr >> 12) as usize;
    RunState::OP_TABLE[opcode](state, instr);
}
