use crate::{
    lexer::{Token, TokenKind},
    symbol::{Flag, Label, LineOffs, Register},
};

/// Assembly intermediate representation, contains starting address and list of instructions
pub struct Air {
    /// Memory address to start program at
    orig: Option<u16>,
    /// AIR
    ast: Vec<AirStmt>,
}

impl Air {
    pub fn new() -> Self {
        Air {
            orig: None,
            ast: Vec::new(),
        }
    }
}

/// Single LC3 statement. Has optional labels.
pub enum AirStmt {
    /// Add SR1 (source register 1) with SR2 and store in DR (destination register)
    Add {
        label: Option<Label>,
        dest_r: Register,
        src_r_1: Register,
        src_r_2: ImmediateOrReg,
    },
    /// Bitwise-and SR1 with SR2 and store in DR
    And {
        label: Option<Label>,
        dest_r: Register,
        src_r_1: Register,
        src_r_2: ImmediateOrReg,
    },
    /// Branch based on flag by adding ByteOffs to PC (program counter)
    BR {
        label: Option<Label>,
        cc: Flag,
        pc_offset9: LineOffs,
    },
    /// Set PC to BR to perform a jump on the next cycle
    JMP {
        label: Option<Label>,
        base_r: Register,
    },
    /// Store current instruction at R7 and jump to provided label
    JSR {
        label: Option<Label>,
        pc_offset11: u16,
    },
    /// Jump to subroutine stored at BR
    JSRR {
        label: Option<Label>,
        base_r: Register,
    },
    /// Load value directly from a memory address into DR
    LD {
        label: Option<Label>,
        dest_r: Register,
        pc_offset9: u16,
    },
    LDI {
        label: Option<Label>,
        dest_r: Register,
        pc_offset9: u16,
    },
    LDR {
        label: Option<Label>,
        dest_r: Register,
        base_r: Register,
        pc_offset6: u16,
    },
    LEA {
        label: Option<Label>,
        dest_r: Register,
        pc_offset9: u16,
    },
    NOT {
        label: Option<Label>,
        dest_r: Register,
        src_r: Register,
    },
    RET {
        label: Option<Label>,
    },
    RTI {
        label: Option<Label>,
    },
    ST {
        label: Option<Label>,
        src_r: Register,
        pc_offset9: u16,
    },
    STI {
        label: Option<Label>,
        src_r: Register,
        pc_offset9: u16,
    },
    RawBytes {
        // Label is not optional for bytes as a sanity check.
        label: Label,
        bytes: Vec<TokenKind>
    },
    Trap {
        label: Option<Label>,
        trap_val: u16,
    }
}

// add and and commands support immediate value
pub(crate) enum ImmediateOrReg {
    Reg(Register),
    Imm5(u8),
}
