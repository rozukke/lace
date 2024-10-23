use core::panic;
use std::{
    cmp::Ordering,
    i16,
    io::{stdin, stdout, IsTerminal, Read, Write},
    u16, u32, u8, usize,
};

use crate::{
    debugger::{Action, Debugger, DebuggerOptions, RelevantInstr},
    dprintln, Air,
};
use colored::Colorize;
use console::Term;
use miette::Result;

/// LC3 can address 128KB of memory.
pub(crate) const MEMORY_MAX: usize = 0x10000;

pub struct RunEnvironment {
    state: RunState,
    debugger: Option<Debugger>,
}

/// Represents complete program state during runtime.
#[derive(Clone)]
pub(super) struct RunState {
    /// System memory - 128KB in size.
    /// Need to figure out if this would cause problems with the stack.
    mem: Box<[u16; MEMORY_MAX]>,
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
pub(super) enum RunFlag {
    N = 0b100,
    Z = 0b010,
    P = 0b001,
    Uninit = 0b000,
}

impl RunEnvironment {
    // Not generic because of miette error
    pub fn try_from(air: Air) -> Result<RunEnvironment> {
        let orig = air.orig().unwrap_or(0x3000);
        let mut air_array: Vec<u16> = Vec::with_capacity(air.len() + 1);

        air_array.push(orig);
        for stmt in air {
            air_array.push(stmt.emit()?);
        }

        RunEnvironment::from_raw(air_array.as_slice())
    }

    pub fn try_from_with_debugger(
        air: Air,
        debugger_opts: DebuggerOptions,
    ) -> Result<RunEnvironment> {
        let mut env = Self::try_from(air)?;
        env.debugger = Some(Debugger::new(debugger_opts, env.state.clone()));
        Ok(env)
    }

    pub fn from_raw(raw: &[u16]) -> Result<RunEnvironment> {
        let orig = raw[0] as usize;
        if orig as usize + raw.len() > MEMORY_MAX {
            panic!("Assembly file is too long and cannot fit in memory.");
        }

        let mut mem = [0; MEMORY_MAX];
        let raw = &raw[1..];

        mem[orig..orig + raw.len()].clone_from_slice(&raw);

        Ok(RunEnvironment {
            state: RunState {
                mem: Box::new(mem),
                pc: orig as u16,
                reg: [0; 8],
                flag: RunFlag::Uninit,
                _psr: 0,
            },
            debugger: None,
        })
    }

    /// Run with preset memory
    pub fn run(&mut self) {
        loop {
            if let Some(debugger) = &mut self.debugger {
                // TODO(feat): This newline is often unneccessary
                // A better option would be to keep track of whether stdout cursor is on a new line
                // And only print \n if not
                dprintln!();
                dprintln!("\nProgram counter at: 0x{:04x}", self.state.pc);
                match debugger.wait_for_action(&mut self.state) {
                    Action::Proceed => (),
                    Action::StopDebugger => {
                        self.debugger = None;
                        continue; // Not technically necessary
                    }
                    Action::ExitProgram => return,
                }
                // If still stuck on HALT
                // Never *execute* HALT while debugger is active
                // Wait for pc to change, such as `reset` command
                // Or `exit` or `quit`
                if RelevantInstr::try_from(self.state.mem[self.state.pc as usize])
                    == Ok(RelevantInstr::TrapHalt)
                {
                    continue;
                }
            }

            if self.state.pc >= 0xFE00 {
                // Entering device address space
                break;
            }
            println!("EXECUTING ONE INSTRUCTION!");
            // TODO(refactor): Could this line be moved to top of loop? It shouldn't cause out of
            // bounds access I don't think.
            let instr = self.state.mem[self.state.pc as usize];
            let opcode = (instr >> 12) as usize;
            // PC incremented before instruction is performed
            self.state.pc += 1;
            RunState::OP_TABLE[opcode](&mut self.state, instr);
        }
    }
}

impl RunState {
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

    #[inline]
    pub(super) fn reg(&mut self, reg: u16) -> &mut u16 {
        // SAFETY: Should only be indexed with values that are & 0b111
        debug_assert!(reg < 8);
        unsafe { self.reg.get_unchecked_mut(reg as usize) }
    }

    #[inline]
    pub(super) fn mem(&mut self, addr: u16) -> &mut u16 {
        // SAFETY: memory fits any u16 index
        unsafe { self.mem.get_unchecked_mut(addr as usize) }
    }

    #[inline]
    pub(super) fn pc(&mut self) -> &mut u16 {
        &mut self.pc
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
                *self.reg(0) = read_input() as u16;
            }
            // out
            0x21 => {
                let chr = (*self.reg(0) & 0xFF) as u8 as char;
                print!("{chr}");
                stdout().flush().unwrap();
            }
            // puts
            0x22 => {
                // could probably rewrite with iterators but idk if worth
                for addr in *self.reg(0).. {
                    let chr_raw = *self.mem(addr);
                    let chr_ascii = (chr_raw & 0xFF) as u8 as char;
                    if chr_ascii == '\0' {
                        break;
                    }
                    print!("{}", chr_ascii);
                }
                stdout().flush().unwrap();
            }
            // in
            0x23 => {
                let ch = read_input();
                *self.reg(0) = ch as u16;
                print!("{}", ch);
                stdout().flush().unwrap();
            }
            // putsp
            0x24 => {
                'string: for addr in *self.reg(0).. {
                    let chr_raw = *self.mem(addr);
                    for chr in [chr_raw >> 8, chr_raw & 0xFF] {
                        let chr_ascii = chr as u8 as char;
                        if chr_ascii == '\0' {
                            break 'string;
                        }
                        print!("{}", chr_ascii);
                    }
                }
                stdout().flush().unwrap();
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

// Read one byte from stdin or unbuffered terminal
fn read_input() -> u8 {
    if stdin().is_terminal() {
        let cons = Term::stdout();
        let ch = cons.read_char().unwrap();
        ch as u8
    } else {
        let mut buf = [0; 1];
        stdin().read_exact(&mut buf).unwrap();
        buf[0]
    }
}
