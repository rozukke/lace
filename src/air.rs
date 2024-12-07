use std::{i16, u16, u32};

use miette::{bail, Result, Severity};

use crate::{
    debugger::Breakpoint,
    symbol::{Flag, Label, Register, Span},
};

/// Assembly intermediate representation, contains starting address and list of instructions
#[derive(Clone)]
pub struct Air {
    /// Memory address to start program at
    orig: Option<u16>,
    /// AIR
    pub ast: Vec<AsmLine>,

    pub breakpoints: Vec<Breakpoint>,

    pub src: &'static str,
}

impl Air {
    pub fn new(src: &'static str) -> Self {
        Air {
            orig: None,
            ast: Vec::new(),
            breakpoints: Vec::new(),
            src,
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

    pub fn orig(&self) -> Option<u16> {
        self.orig
    }

    pub fn add_stmt(&mut self, stmt: AirStmt, span: Span) {
        self.ast
            .push(AsmLine::new((self.ast.len() + 1) as u16, stmt, span))
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

impl<'a> IntoIterator for &'a Air {
    type Item = &'a AsmLine;
    type IntoIter = std::slice::Iter<'a, AsmLine>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.ast).into_iter()
    }
}

/// Single LC3 statement. Has optional labels.
#[derive(PartialEq, Eq, Debug, Clone)]
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
    Branch { flag: Flag, dest_label: Label },
    /// Jump to address stored in src_reg
    Jump { src_reg: Register },
    /// Store current instruction address in R7 and jump to dest_label
    JumbSub { dest_label: Label },
    /// Jump to subroutine address stored at src_reg
    JumpSubReg { src_reg: Register },
    /// Load value directly from src_label into dest
    Load { dest: Register, src_label: Label },
    /// Load to dest from src_label by dereferencing address stored there
    LoadInd { dest: Register, src_label: Label },
    /// Load value stored at address(src_reg + offset) to dest
    LoadOffs {
        dest: Register,
        src_reg: Register,
        offset: u8,
    },
    /// Load address identified by src_label into dest
    LoadEAddr { dest: Register, src_label: Label },
    /// Bitwise-not value at src_reg and store in dest
    Not { dest: Register, src_reg: Register },
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
    /// Store value in src_reg at dest_label + offset
    StoreOffs {
        src_reg: Register,
        dest_reg: Register,
        offset: u8,
    },
    /// Push onto stack (extended dialect)
    Push { src_reg: Register },
    /// Pop from stack (extended dialect)
    Pop { dest_reg: Register },
    /// Jump to subroutine and push onto stack (extended dialect)
    Call { dest_label: Label },
    /// Return from subroutine using stack (extended dialect)
    Rets,
    /// A raw value created during preprocessing
    RawWord { val: RawWord },
    /// Jump to address at index trap_vect of the trap table
    Trap { trap_vect: u8 },
}

/// Used for ADD and AND commands as they support either 5-bit immediate values or registers as the
/// last operand.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ImmediateOrReg {
    Reg(Register),
    Imm5(u8),
}

impl ImmediateOrReg {
    pub fn bits(&self) -> u16 {
        match self {
            Self::Reg(reg) => *reg as u16,
            // Bit to show that the value is not a register
            Self::Imm5(val) => (*val as u16) & 0b11111 | 0b100000,
        }
    }
}

/// Newtype to represent 16 bits (word size of LC3) as a raw value.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RawWord(pub u16);

/// A line (16 bits) of assembly.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct AsmLine {
    pub line: u16,
    pub stmt: AirStmt,
    pub span: Span,
}

impl AsmLine {
    pub fn new(line: u16, stmt: AirStmt, span: Span) -> Self {
        AsmLine { line, stmt, span }
    }

    /// Fill label references using values from symbol table
    pub fn backpatch(&mut self) -> Result<()> {
        let inner_label = match self.stmt {
            AirStmt::Branch {
                ref mut dest_label, ..
            } => dest_label,
            AirStmt::JumbSub { ref mut dest_label } => dest_label,
            AirStmt::Load {
                ref mut src_label, ..
            } => src_label,
            AirStmt::LoadInd {
                ref mut src_label, ..
            } => src_label,
            AirStmt::LoadEAddr {
                ref mut src_label, ..
            } => src_label,
            AirStmt::Store {
                ref mut dest_label, ..
            } => dest_label,
            AirStmt::StoreInd {
                ref mut dest_label, ..
            } => dest_label,
            AirStmt::Call { ref mut dest_label } => dest_label,
            _ => return Ok(()),
        };
        *inner_label = inner_label.clone().filled()?;
        Ok(())
    }

    /// Return binary representation of a statement
    #[allow(unused)]
    pub fn emit(&self) -> Result<u16> {
        match &self.stmt {
            AirStmt::Add {
                dest,
                src_reg,
                src_reg_imm,
            } => {
                // Opcode
                let mut raw = 0x1000;
                // Destination register
                raw |= (*dest as u16) << 9;
                // Source register
                raw |= (*src_reg as u16) << 6;
                // Source register or immediate
                raw |= src_reg_imm.bits();
                Ok(raw)
            }
            AirStmt::And {
                dest,
                src_reg,
                src_reg_imm,
            } => {
                let mut raw = 0x5000;
                raw |= (*dest as u16) << 9;
                raw |= (*src_reg as u16) << 6;
                raw |= src_reg_imm.bits();
                Ok(raw)
            }
            AirStmt::Branch { flag, dest_label } => {
                let mut raw = 0x0000;
                raw |= flag.bits() << 9;
                // Label into offset
                raw |= self.bit_offs(dest_label, 9)?;
                Ok(raw)
            }
            AirStmt::Jump { src_reg } => {
                let mut raw = 0xC000;
                raw |= (*src_reg as u16) << 6;
                Ok(raw)
            }
            AirStmt::JumbSub { dest_label } => {
                let mut raw = 0x4800;
                raw |= self.bit_offs(dest_label, 11)?;
                Ok(raw)
            }
            AirStmt::JumpSubReg { src_reg } => {
                let mut raw = 0x4000;
                raw |= (*src_reg as u16) << 6;
                Ok(raw)
            }
            AirStmt::Load { dest, src_label } => {
                let mut raw = 0x2000;
                raw |= (*dest as u16) << 9;
                raw |= self.bit_offs(src_label, 9)?;
                Ok(raw)
            }
            AirStmt::LoadInd { dest, src_label } => {
                let mut raw = 0xA000;
                raw |= (*dest as u16) << 9;
                raw |= self.bit_offs(src_label, 9)?;
                Ok(raw)
            }
            AirStmt::LoadOffs {
                dest,
                src_reg,
                offset,
            } => {
                let mut raw = 0x6000;
                raw |= (*dest as u16) << 9;
                raw |= (*src_reg as u16) << 6;
                raw |= *offset as u16;
                Ok(raw)
            }
            AirStmt::LoadEAddr { dest, src_label } => {
                let mut raw = 0xE000;
                raw |= (*dest as u16) << 9;
                raw |= self.bit_offs(src_label, 9)?;
                Ok(raw)
            }
            AirStmt::Not { dest, src_reg } => {
                let mut raw = 0x9000;
                raw |= (*dest as u16) << 9;
                raw |= (*src_reg as u16) << 6;
                // Spec requirement
                raw |= 0b111111;
                Ok(raw)
            }
            AirStmt::Return => Ok(0xC1C0),
            AirStmt::Interrupt => Ok(0x8000),
            AirStmt::Store {
                src_reg,
                dest_label,
            } => {
                let mut raw = 0x3000;
                raw |= (*src_reg as u16) << 9;
                raw |= self.bit_offs(dest_label, 9)?;
                Ok(raw)
            }
            AirStmt::StoreInd {
                src_reg,
                dest_label,
            } => {
                let mut raw = 0xB000;
                raw |= (*src_reg as u16) << 9;
                raw |= self.bit_offs(dest_label, 9)?;
                Ok(raw)
            }
            AirStmt::StoreOffs {
                src_reg,
                dest_reg,
                offset,
            } => {
                let mut raw = 0x7000;
                raw |= (*src_reg as u16) << 9;
                raw |= (*dest_reg as u16) << 6;
                raw |= *offset as u16;
                Ok(raw)
            }
            // In order to be able to do push, pop, call and rets with the same instruction, a new format
            // must be used where there are two specifier bits (one to specify if it's subroutine call related,
            // and the second to specify if it's a push operation, i.e. call and push) as below:
            //
            //  1     2  3  4  5    6
            // [1101][1][1][0][000][000000]
            //
            // Explanation:
            // 1. Opcode
            // 2. 1 if call/rets, 0 if push/pop
            // 3. 1 if call/push, 0 if rets/pop
            // 4. Offset starts here when call
            // 5. Base pointer located here when push/pop, continued offset when call
            // 6. Continued offset when call
            //
            // There are 10 bits of offset precision when using a call instruction.
            // There also isn't really a way to work around this setup if other instructions
            // are to be left untouched.
            AirStmt::Push { src_reg } => {
                let mut raw = 0xD000;
                raw |= 0x0400;
                raw |= (*src_reg as u16) << 6;
                Ok(raw)
            }
            AirStmt::Pop { dest_reg } => {
                let mut raw = 0xD000;
                raw |= (*dest_reg as u16) << 6;
                Ok(raw)
            }
            AirStmt::Call { dest_label } => {
                let mut raw = 0xD000;
                raw |= 0x0C00;
                raw |= self.bit_offs(dest_label, 10)?;
                Ok(raw)
            }
            AirStmt::Rets => {
                let mut raw = 0xD000;
                raw |= 0x0800;
                Ok(raw)
            }
            AirStmt::RawWord { val: bytes } => Ok(bytes.0),
            AirStmt::Trap { trap_vect } => Ok(0xF000 | *trap_vect as u16),
        }
    }

    /// Find offset between label reference and current line while checking bounds
    fn bit_offs(&self, ref_label: &Label, bits: u32) -> Result<u16> {
        let label_pos = match ref_label {
            Label::Ref(val) => val,
            Label::Unfilled(_) => panic!("Tried to offset unfilled label"),
        };
        let (offset, _) = label_pos.overflowing_sub(self.line);
        let offset = (offset as i16) - 1;
        // Must fit in specified offset bits
        if offset.abs() > 2i16.pow(bits - 1) - if offset > 0 { 1 } else { 0 } {
            bail!(
                severity = Severity::Error,
                r#"Difference between label and label reference is too large: at line {}, referencing line {}
                Please note that this could be because of a long .stringz literal or large .blkw allocation."#,
                self.line,
                label_pos
            )
        }
        Ok((offset as u16) & (2u16.pow(bits) - 1))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        air::AirStmt,
        parser::AsmParser,
        symbol::{Flag, SrcOffset},
    };

    // Backpatching tests
    #[test]
    fn backpatch() {
        let mut air = AsmParser::new(
            r#"
        br label
        label jmp r0
        "#,
        )
        .unwrap()
        .parse()
        .unwrap();
        air.backpatch().unwrap();
        assert_eq!(air.len(), 2);

        assert_eq!(
            air.get(0),
            &AsmLine {
                line: 1,
                stmt: AirStmt::Branch {
                    flag: Flag::Nzp,
                    dest_label: Label::Ref(2)
                },
                span: Span::new(
                    SrcOffset(
                        r#"
        "#
                        .len()
                    ),
                    "br label".len()
                )
            }
        );
    }

    #[test]
    fn backpatch_missing() {
        let mut air = AsmParser::new("br label").unwrap().parse().unwrap();
        assert!(air.backpatch().is_err());
    }

    // Code emission tests
    #[test]
    fn emit_add_reg() {
        let asm = AsmLine {
            line: 0,
            stmt: AirStmt::Add {
                dest: Register::R1,
                src_reg: Register::R2,
                src_reg_imm: ImmediateOrReg::Reg(Register::R3),
            },
            span: Span::dummy(),
        };
        assert_eq!(asm.emit().unwrap(), 0x1283)
    }

    #[test]
    fn emit_add_imm() {
        let asm = AsmLine {
            line: 0,
            stmt: AirStmt::Add {
                dest: Register::R1,
                src_reg: Register::R2,
                src_reg_imm: ImmediateOrReg::Imm5(0b01111),
            },
            span: Span::dummy(),
        };
        assert_eq!(asm.emit().unwrap(), 0x12AF)
    }

    #[test]
    fn emit_label() {
        let asm = AsmLine {
            line: 1,
            stmt: AirStmt::Branch {
                flag: Flag::Nzp,
                dest_label: Label::Ref(4),
            },
            span: Span::dummy(),
        };
        assert_eq!(asm.emit().unwrap(), 0b0000111000000010)
    }

    #[test]
    fn emit_label_neg() {
        let asm = AsmLine {
            line: 4,
            stmt: AirStmt::Branch {
                flag: Flag::Nzp,
                dest_label: Label::Ref(1),
            },
            span: Span::dummy(),
        };
        assert_eq!(asm.emit().unwrap(), 0b0000111111111100)
    }

    #[test]
    fn emit_label_bad_range() {
        let asm = AsmLine {
            line: 1,
            stmt: AirStmt::Branch {
                flag: Flag::Nzp,
                dest_label: Label::Ref(258),
            },
            span: Span::dummy(),
        };
        assert!(asm.emit().is_err());
        let asm = AsmLine {
            line: 257,
            stmt: AirStmt::Branch {
                flag: Flag::Nzp,
                dest_label: Label::Ref(1),
            },
            span: Span::dummy(),
        };
        assert!(asm.emit().is_err())
    }

    // Regression
    #[test]
    fn emit_neg_imm() {
        let asm = AsmLine {
            line: 1,
            stmt: AirStmt::Add {
                dest: Register::R4,
                src_reg: Register::R4,
                src_reg_imm: ImmediateOrReg::Imm5((-1i8) as u8),
            },
            span: Span::dummy(),
        };
        assert_eq!(asm.emit().unwrap(), 0x193f);
    }
}
