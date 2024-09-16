use core::panic;
use std::{cmp::Ordering, i16, io::Write, u16, u32, u8, usize};

use crate::Air;
use colored::Colorize;
use console::Term;
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
    _psr: u16,
}

#[derive(Clone, Copy)]
enum RunFlag {
    N = 0b100,
    Z = 0b010,
    P = 0b001,
    Uninit = 0b111,
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
            _psr: 0,
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

    /// For testing
    pub fn inspect(&self, mem: u16) -> u16 {
        unsafe { *self.mem.get_unchecked(mem as usize) }
    }

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

        let val1 = *self.reg(sr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            *self.reg(instr & 0b111)
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

        let val1 = *self.reg(sr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            *self.reg(instr & 0b111)
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

    fn rti(&mut self, _instr: u16) {
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
        let val = *self.reg(sr);
        *self.mem(ptr.wrapping_add(Self::s_ext(instr, 6))) = val;
    }

    fn trap(&mut self, instr: u16) {
        let trap_vect = instr & 0xFF;
        match trap_vect {
            // getc
            0x20 => {
                let cons = Term::stdout();
                let c = cons.read_char().unwrap();
                *self.reg(0) = c as u16;
            }
            // out
            0x21 => {
                let chr = (*self.reg(0) & 0xFF) as u8 as char;
                print!("{chr}");
            }
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
                let mut cons = Term::stdout();
                let c = cons.read_char().unwrap();
                *self.reg(0) = c as u16;
                write!(cons, "{c}").unwrap();
                cons.flush().unwrap();
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

#[cfg(test)]
mod test {
    use crate::{AsmParser, RunState};

    fn run(src: &'static str) -> RunState {
        let parser = AsmParser::new(src).unwrap();
        let mut air = parser.parse().unwrap();
        air.backpatch().unwrap();
        let mut state = RunState::try_from(air).unwrap();
        state.run();
        state
    }

    #[test]
    fn run_add() {
        let mut state = run(r#"
        add r0 r0 #15
        add r1 r0 #5
        add r2 r0 r1
        add r3 r3 #2
        add r3 r3 r3
        "#);
        assert_eq!(*state.reg(0), 15);
        assert_eq!(*state.reg(1), 20);
        assert_eq!(*state.reg(2), 35);
        assert_eq!(*state.reg(3), 4);
    }

    #[test]
    fn run_and() {
        let mut state = run(r#"
        ; preset value
        add r0 r0 #15

        and r0 r0 #7
        and r1 r0 #4
        and r2 r0 r1
        add r3 r3 #9
        and r3 r3 r3
        "#);
        assert_eq!(*state.reg(0), 7);
        assert_eq!(*state.reg(1), 4);
        assert_eq!(*state.reg(2), 4);
        assert_eq!(*state.reg(3), 9);
    }

    #[test]
    fn run_br() {
        let mut state = run(r#"
        br next
        add r0 r0 #12
        first
        add r1 r1 #13
        br end
        next
        add r0 r0 #11
        br first
        end halt
        "#);
        assert_eq!(*state.reg(0), 11);
        assert_eq!(*state.reg(1), 13);
    }

    #[test]
    fn run_jmp() {
        let mut state = run(r#"
        lea r1 end
        jmp r1
        add r0 r0 #15
        end halt
        "#);
        assert_eq!(*state.reg(0), 0);
    }

    #[test]
    fn run_st() {
        let state = run(r#"
        add r0 r0 #12
        st r0 val
        val .blkw #1
        "#);
        assert_eq!(state.inspect(0x3002), 12);
    }

    #[test]
    fn run_ld() {
        let mut state = run(r#"
        ld r0 val
        halt
        val .fill 0x300
        "#);
        assert_eq!(*state.reg(0), 0x300);
    }
}
