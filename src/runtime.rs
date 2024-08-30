use crate::symbol::Flag;

/// LC3 can address 128KB of memory.
const MEMORY_MAX: usize = 0x10000;

/// Represents complete program state during runtime.
pub struct State {
    /// System memory - 128KB in size.
    /// Need to figure out if this would cause problems with the stack.
    mem: [u16; MEMORY_MAX],
    /// Address provided by program to indicate where to load user code
    init_addr: u16,
    /// Program counter
    pc: u16,
    /// 8x 16-bit registers
    reg: [u16; 8],
    /// Condition code
    cc: Flag,
    /// Processor status register
    psr: u16,
}
