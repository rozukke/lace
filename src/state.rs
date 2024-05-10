use std::collections::HashMap;

const MEMORY_MAX: usize = 0xFFFF;

// Represents complete program state
// Of note - instructions are not stored in `mem`, but rather mapped to it.
pub struct State {
    // 128KB of system memory
    mem: [u16; MEMORY_MAX],
    // Address provided by program to indicate where to load user code
    init_addr: u16,
    // Program counter
    pc: u16,
    // 8x 16-bit registers
    reg: [u16; 8],
    // Condition code
    cc: Flag,
    // Processor status register
    psr: u16,
}

// Set using result from previous instruction
pub enum Flag {
    // Negative
    N,
    // Zero
    Z,
    // Positive
    P,
}

struct SymbolTable {
    // Hashmap of LABEL -> address
    table: HashMap<String, u16>,
}
