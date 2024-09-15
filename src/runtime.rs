#![allow(unused)]
use core::panic;
use std::{cmp::Ordering, i16, u16, u32, u8, usize};

use crate::{symbol::Flag, Air};
use colored::Colorize;
use miette::Result;

/// LC3 can address 128KB of memory.
const MEMORY_MAX: usize = 0x10000;

/// Represents complete program state during runtime.
pub struct RunState {
    /// System memory - 128KB in size.
    /// Need to figure out if this would cause problems with the stack.
    mem: [u16; MEMORY_MAX],
    /// Program counter
    pc: u16,
    /// 8x 16-bit registers
    reg: [u16; 8],
    /// Condition code
    flag: RunFlag,
    /// Processor status register
    psr: u16,
}

#[derive(Clone, Copy)]
enum RunFlag {
    N = 0b100,
    Z = 0b010,
    P = 0b001,
    Uninit = 0b000,
}

impl RunState {
    // Not generic because of miette error
    pub fn try_from(air: Air) -> Result<RunState> {
        let orig: usize = air.orig().unwrap_or(0x3000).into();
        let mut mem = [0; MEMORY_MAX];
        let mut air_array = Vec::with_capacity(air.len());

        for stmt in air {
            air_array.push(stmt.emit()?);
        }

        // Sanity check
        if orig + air_array.len() > MEMORY_MAX {
            panic!("Assembly file is too long and cannot fit in memory.");
        }

        mem[orig..orig + air_array.len()].clone_from_slice(&air_array);

        Ok(RunState {
            mem,
            pc: orig as u16,
            reg: [0; 8],
            flag: RunFlag::Uninit,
            psr: 0,
        })
    }

    const OP_TABLE: [fn(&mut RunState, u16); 16] = [
        Self::br,   // 0x0
        Self::add,  // 0x1
        Self::ld,   // 0x2
        Self::st,   // 0x3
        Self::jsr,  // 0x4
        Self::and,  // 0x5
        Self::ldr,  // 0x6
        Self::str,  // 0x7
        Self::rti,  // 0x8
        Self::not,  // 0x9
        Self::ldi,  // 0xA
        Self::sti,  // 0xB
        Self::jmp,  // 0xC
        Self::nul,  // 0xD
        Self::lea,  // 0xE
        Self::trap, // 0xF
    ];

    /// Run with preset memory
    pub fn run(&mut self) {
        loop {
            if self.pc >= 0xFE00 {
                // Entering device address space
                break;
            }
            let instr = self.mem[self.pc as usize];
            let opcode = (instr >> 12) as usize;
            // PC incremented before instruction is performed
            self.pc += 1;
            Self::OP_TABLE[opcode](self, instr);
        }
    }

    #[inline]
    fn reg(&mut self, reg: u16) -> &mut u16 {
        // SAFETY: Should only be indexed with values that are & 0b111
        unsafe { self.reg.get_unchecked_mut(reg as usize) }
    }

    #[inline]
    fn mem(&mut self, addr: u16) -> &mut u16 {
        // SAFETY: memory fits any u16 index
        unsafe { self.mem.get_unchecked_mut(addr as usize) }
    }

    #[inline]
    fn s_ext(val: u16, bits: u32) -> u16 {
        let val = val & (2u16.pow(bits) - 1);
        if val & 2u16.pow(bits - 1) == 0 {
            // positive
            val
        } else {
            // negative
            val | !(2u16.pow(bits) - 1)
        }
    }

    #[inline]
    fn set_flags(&mut self, val: u16) {
        self.flag = match (val as i16).cmp(&0) {
            Ordering::Less => RunFlag::N,
            Ordering::Equal => RunFlag::Z,
            Ordering::Greater => RunFlag::P,
        }
    }

    fn nul(&mut self, _instr: u16) {
        panic!("You called a reserved instruction. Halting...")
    }

    fn add(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;

        let val1 = *self.reg(dr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            *self.reg((instr & 0b111))
        } else {
            // imm
            Self::s_ext(instr, 5)
        };
        let res = val1.wrapping_add(val2);
        self.set_flags(res);
        *self.reg(dr) = res;
    }

    fn and(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;

        let val1 = *self.reg(dr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            *self.reg((instr & 0b111))
        } else {
            // imm
            Self::s_ext(instr, 5)
        };
        let res = val1 & val2;
        self.set_flags(res);
        *self.reg(dr) = res;
    }

    fn br(&mut self, instr: u16) {
        let flag = (instr >> 9) & 0b111;
        if self.flag as u16 & flag != 0 {
            self.pc = self.pc.wrapping_add(Self::s_ext(instr, 9))
        }
    }

    fn jmp(&mut self, instr: u16) {
        let br = (instr >> 6) & 0b111;
        self.pc = *self.reg(br)
    }

    fn jsr(&mut self, instr: u16) {
        *self.reg(7) = self.pc;
        if instr & 0x800 == 0 {
            // reg
            let br = (instr >> 6) & 0b111;
            self.pc = *self.reg(br)
        } else {
            // offs
            self.pc = self.pc.wrapping_add(Self::s_ext(instr, 11))
        }
    }

    fn ld(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let val = *self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        *self.reg(dr) = val;
        self.set_flags(val);
    }

    fn ldi(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let ptr = *self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        let val = *self.mem(ptr);
        *self.reg(dr) = val;
        self.set_flags(val);
    }

    fn ldr(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let br = (instr >> 6) & 0b111;
        let ptr = *self.reg(br);
        let val = *self.mem(ptr.wrapping_add(Self::s_ext(instr, 6)));
        *self.reg(dr) = val;
        self.set_flags(val);
    }

    fn lea(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let val = self.pc.wrapping_add(Self::s_ext(instr, 9));
        *self.reg(dr) = val;
        self.set_flags(val);
    }

    fn not(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;
        let val = !*self.reg(sr);
        *self.reg(dr) = val;
        self.set_flags(val);
    }

    fn rti(&mut self, instr: u16) {
        todo!("Please open an issue and I'll get RTI implemented in a jiffy :)")
    }

    fn st(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let val = *self.reg(sr);
        *self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9))) = val;
    }

    fn sti(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let val = *self.reg(sr);
        let ptr = *self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        *self.mem(ptr) = val;
    }

    fn str(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let br = (instr >> 6) & 0b111;
        let ptr = *self.reg(br);
        *self.mem(ptr.wrapping_add(Self::s_ext(instr, 6)));
    }

    fn trap(&mut self, instr: u16) {
        let trap_vect = instr & 0xFF;
        match trap_vect {
            // getc
            0x20 => {
                // TODO: impl getc
                todo!("TODO: Some dependencies are required for immediate response.")
            }
            // out
            0x21 => unsafe {
                let chr = (*self.reg(0) & 0xFF) as u8 as char;
                print!("{chr}")
            },
            // puts
            0x22 => {
                // could probably rewrite with iterators but idk if worth
                let mut addr = *self.reg(0);
                let mut string = String::new();
                loop {
                    let chr_raw = *self.mem(addr);
                    let chr_ascii = (chr_raw & 0xFF) as u8 as char;
                    if chr_ascii == '\0' {
                        break;
                    }
                    string.push(chr_ascii);
                    addr += 1;
                }
                print!("{string}");
            }
            // in
            0x23 => {
                // TODO: impl in
                todo!("TODO: Some dependencies are required for immediate response.")
            }
            // putsp
            0x24 => {
                // TODO: impl putsp
                todo!("TODO: putsp can be put off until someone needs it")
            }
            // halt
            0x25 => {
                self.pc = u16::MAX;
                println!("\n{:>12}", "Halted".cyan());
            }
            // putn
            0x26 => {
                let val = *self.reg(0);
                println!("{val}");
            }
            // reg
            0x27 => {
                println!("\n------ Registers ------");
                for (i, reg) in self.reg.iter().enumerate() {
                    println!("r{i}: {reg:.>#19}");
                    // println!("r{i}: {reg:.>#19b}");
                }
                println!("-----------------------");
            }
            // unknown
            _ => panic!("You called a trap with an unknown vector of {}", trap_vect),
        }
    }
}
