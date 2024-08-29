use crate::{
    lexer::Token,
    symbol::{Flag, Label, LineOffs, Register},
};

/// Basically the entire 'AST' when it comes to LC3.
/// TODO: Convert to labels instead of offsets at this stage.
#[allow(clippy::upper_case_acronyms)]
pub enum Op {
    /// Add SR1 (source register 1) with SR2 and store in DR (destination register)
    ADD {
        dest_r: Register,
        src_r_1: Register,
        src_r_2: Choice,
    },
    /// Bitwise-and SR1 with SR2 and store in DR
    AND {
        dest_r: Register,
        src_r_1: Register,
        src_r_2: Choice,
    },
    /// Branch based on flag by adding ByteOffs to PC (program counter)
    BR {
        cc: Flag,
        pc_offset9: LineOffs,
    },
    /// Set PC to BR to perform a jump on the next cycle
    JMP {
        base_r: Register,
    },
    /// Store current instruction at R7 and jump to provided label
    JSR {
        pc_offset11: u16,
    },
    /// Jump to subroutine stored at BR
    JSRR {
        base_r: Register,
    },
    /// Load value directly from a memory address into DR
    LD {
        dest_r: Register,
        pc_offset9: u16,
    },
    LDI {
        dest_r: Register,
        pc_offset9: u16,
    },
    LDR {
        dest_r: Register,
        base_r: Register,
        pc_offset6: u16,
    },
    LEA {
        dest_r: Register,
        pc_offset9: u16,
    },
    NOT {
        dest_r: Register,
        src_r: Register,
    },
    RET,
    RTI,
    ST {
        src_r: Register,
        pc_offset9: u16,
    },
    STI {
        src_r: Register,
        pc_offset9: u16,
    },
    Dir {
        args: Option<Vec<Token>>,
    },
}

// ADD and AND commands support immediate value
pub(crate) enum Choice {
    Reg(Register),
    Imm5(u8),
}
