use miette::Result;

use crate::{
    air::AirStmt,
    lexer::{cursor::Cursor, TokenKind},
    runtime::RunState,
    symbol::InstrKind,
    AsmParser,
};

pub fn run(state: &mut RunState, line: String) {
    let stmt = parse(line);

    println!("{:#?}", stmt);

    todo!();
}

fn parse(line: String) -> Result<AirStmt> {
    // TODO(fix): This is a TERRIBLE solution. Ideally cursor doesn't take &'static str ??
    let line = Box::leak(line.into_boxed_str());

    let mut parser = AsmParser::new_simple(line)?;

    let stmt = parser.parse_simple()?;

    println!("{:#?}", stmt);

    todo!();
}
