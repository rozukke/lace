use std::{mem::{replace, swap}, u16};

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
    ast: Vec<AsmLine>,
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
        self.ast.push(AsmLine::new((self.ast.len() + 1) as u16, stmt))
    }

    pub fn get(&self, idx: usize) -> &AsmLine {
        &self.ast[idx]
    }

    pub fn len(&self) -> usize {
        self.ast.len()
    }

    pub fn backpatch(&mut self) -> Result<()> {
        for stmt in self.ast.iter_mut() {
            stmt.backpatch()?;
        }
        Ok(())
    }
}

/// Single LC3 statement. Has optional labels.
#[derive(PartialEq, Eq, Debug)]
pub enum AirStmt {
    /// Add src_reg with src_reg_imm and store in dest
    Add {
        dest: Register,
        src_reg: Register,
        src_reg_imm: ImmediateOrReg,
    },
    /// Bitwise-and src_reg with src_reg_imm and store in dest
    And {
        dest: Register,
        src_reg: Register,
        src_reg_imm: ImmediateOrReg,
    },
    /// Branch based on flag by jumping to dest_label
    Branch {
        flag: Flag,
        dest_label: Label,
    },
    /// Jump to address stored in src_reg
    Jump {
        src_reg: Register,
    },
    /// Store current instruction address in R7 and jump to dest_label
    JumbSub {
        dest_label: Label,
    },
    /// Jump to subroutine address stored at src_reg
    JumpSubReg {
        src_reg: Register,
    },
    /// Load value directly from src_label into dest
    Load {
        dest: Register,
        src_label: Label,
    },
    /// Load to dest from src_label by dereferencing address stored there
    LoadInd {
        dest: Register,
        src_label: Label,
    },
    /// Load value stored at address(src_reg + offset) to dest
    LoadOffs {
        dest: Register,
        src_reg: Register,
        offset: u8,
    },
    /// Load address identified by src_label into dest
    LoadEAddr {
        dest: Register,
        src_label: Label,
    },
    /// Bitwise-not value at src_reg and store in dest
    Not {
        dest: Register,
        src_reg: Register,
    },
    /// Jump to address stored in R7
    Return,
    /// Idk
    Interrupt,
    /// Store value in src_reg at address identified by dest_label
    Store {
        src_reg: Register,
        dest_label: Label,
    },
    /// Store value in src_reg at address dereferenced from dest_label
    StoreInd {
        src_reg: Register,
        dest_label: Label,
    },
    /// A raw value created during preprocessing
    RawWord {
        bytes: RawWord,
    },
    /// Jump to address at index trap_vect of the trap table
    Trap {
        trap_vect: u8,
    },
}

/// Used for ADD and AND commands as they support either 5-bit immediate values or registers as the
/// last operand.
#[derive(PartialEq, Eq, Debug)]
pub(crate) enum ImmediateOrReg {
    Reg(Register),
    Imm5(u8),
}

/// Newtype to represent 16 bits (word size of LC3) as a raw value.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RawWord(pub u16);

/// A line (16 bits) of assembly.
#[derive(PartialEq, Eq, Debug)]
pub struct AsmLine {
    pub line: u16,
    pub stmt: AirStmt,
}

impl AsmLine {
    pub fn new(line: u16, stmt: AirStmt) -> Self {
        AsmLine { line, stmt }
    }

    /// Fill label references using values from symbol table
    pub fn backpatch(&mut self) -> Result<()> {
        let inner_label = match self.stmt {
            AirStmt::Branch { ref mut dest_label, .. } => dest_label,
            AirStmt::JumbSub { ref mut dest_label } => dest_label,
            AirStmt::Load { ref mut src_label, .. } => src_label,
            AirStmt::LoadInd { ref mut src_label, .. } => src_label,
            AirStmt::LoadEAddr { ref mut src_label, .. } => src_label,
            AirStmt::Store { ref mut dest_label, .. } => dest_label,
            AirStmt::StoreInd { ref mut dest_label, .. } => dest_label,
            _ => return Ok(()),
        };
        *inner_label = inner_label.clone().filled()?;
        Ok(())
    }
}

mod tests {
    use crate::{air::AirStmt, parser::AsmParser, symbol::Flag};
    use super::*;

    #[test]
    fn backpatch() {
        let mut air = AsmParser::new(r#"
        br label
        label jmp r0
        "#).unwrap().parse().unwrap();
        air.backpatch().unwrap();
        assert_eq!(air.len(), 2);

        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Branch {
                    flag: Flag::Nzp,
                    dest_label: Label::Ref(2)
                }
            }
        );
    }

    fn backpatch_missing() {
        let mut air = AsmParser::new("br label").unwrap().parse().unwrap();
        assert!(air.backpatch().is_err());
    }
}
