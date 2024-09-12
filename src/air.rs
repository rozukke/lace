use miette::{bail, LabeledSpan, Result, Severity};

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

    /// Set the .orig offset for the program. Error if set twice.
    pub fn set_orig(&mut self, val: u16) -> Result<()> {
        if let Some(_) = self.orig {
            bail!("Origin set twice.")
        } else {
            self.orig = Some(val);
            Ok(())
        }
    }

    pub fn add_stmt(&mut self, stmt: AirStmt) {
        self.ast.push(stmt)
    }

    pub fn backpatch(&mut self) {
        // Use labels filled during parsing to resolve unfilled labels
        for stmt in &self.ast {
            todo!()
        }
    }

    pub fn get(&self, idx: usize) -> &AirStmt {
        &self.ast[idx]
    }

    pub fn len(&self) -> usize {
        self.ast.len()
    }
}

/// Single LC3 statement. Has optional labels.
#[derive(PartialEq, Eq, Debug)]
pub enum AirStmt {
    /// Add SR1 (source register 1) with SR2 and store in DR (destination register)
    Add {
        label: Option<Label>,
        dest: Register,
        src_reg: Register,
        src_reg_imm: ImmediateOrReg,
    },
    /// Bitwise-and SR1 with SR2 and store in DR
    And {
        label: Option<Label>,
        dest: Register,
        src_reg: Register,
        src_reg_imm: ImmediateOrReg,
    },
    /// Branch based on flag by adding ByteOffs to PC (program counter)
    Branch {
        label: Option<Label>,
        flag: Flag,
        dest_label: Label,
    },
    /// Set PC to BR to perform a jump on the next cycle
    Jump {
        label: Option<Label>,
        src_reg: Register,
    },
    /// Store current instruction at R7 and jump to provided label
    JumbSub {
        label: Option<Label>,
        dest_label: Label,
    },
    /// Jump to subroutine stored at BR
    JumpSubReg {
        label: Option<Label>,
        src_reg: Register,
    },
    /// Load value directly from a memory address into DR
    Load {
        label: Option<Label>,
        dest: Register,
        src_label: Label,
    },
    LoadInd {
        label: Option<Label>,
        dest: Register,
        src_label: Label,
    },
    LoadOffs {
        label: Option<Label>,
        dest: Register,
        src_reg: Register,
        offset: u16,
    },
    LoadEAddr {
        label: Option<Label>,
        dest: Register,
        src_label: Label,
    },
    Not {
        label: Option<Label>,
        dest: Register,
        src_reg: Register,
    },
    Return {
        label: Option<Label>,
    },
    Interrupt {
        label: Option<Label>,
    },
    Store {
        label: Option<Label>,
        src_reg: Register,
        dest_label: Label,
    },
    StoreInd {
        label: Option<Label>,
        src_reg: Register,
        dest_label: Label,
    },
    RawWord {
        // Label is optional as each byte is its own line, checking needs to be done during
        // preprocessing.
        label: Option<Label>,
        bytes: RawWord,
    },
    Trap {
        label: Option<Label>,
        trap_vect: u8,
    },
}

// add and and commands support immediate value
#[derive(PartialEq, Eq, Debug)]
pub(crate) enum ImmediateOrReg {
    Reg(Register),
    Imm5(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RawWord(pub u16);
