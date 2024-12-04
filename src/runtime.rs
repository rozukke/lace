use std::{
    cmp::Ordering,
    i16,
    io::{stdin, stdout, IsTerminal, Read, Write},
    u16, u32, u8, usize,
};

use crate::{
    debugger::{Action, Debugger, DebuggerOptions, RelevantInstr},
    dprintln, env,
    output::Output,
    print_char, Air,
};
use colored::Colorize;
use console::Term;
use miette::Result;

macro_rules! exception {
    ( $fmt:literal $($tt:tt)* ) => {{
        eprintln!(
            concat!("exception: ", $fmt, ", exiting")
            $($tt)*
        );
        std::process::exit(0xEE);
    }};
}

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
    pub fn try_from(air: &Air) -> Result<RunEnvironment> {
        let orig = air.orig().unwrap_or(0x3000);
        let mut air_array: Vec<u16> = Vec::with_capacity(air.len() + 1);

        air_array.push(orig);
        for stmt in air {
            air_array.push(stmt.emit()?);
        }

        RunEnvironment::from_raw(air_array.as_slice())
    }

    pub fn try_from_with_debugger(
        air: Air, // Takes ownership of breakpoints
        debugger_opts: DebuggerOptions,
    ) -> Result<RunEnvironment> {
        let mut env = Self::try_from(&air)?;

        // Add orig to each breakpoint
        let orig = env.state.pc;
        let mut breakpoints = air.breakpoints;
        for breakpoint in breakpoints.iter_mut() {
            *breakpoint += orig;
        }

        env.debugger = Some(Debugger::new(debugger_opts, env.state.clone(), breakpoints));

        Ok(env)
    }

    pub fn from_raw(raw: &[u16]) -> Result<RunEnvironment> {
        let orig = raw[0] as usize;
        if orig as usize + raw.len() > MEMORY_MAX {
            exception!("assembly file is too long and cannot fit in memory");
        }

        let mut mem = [0; MEMORY_MAX];
        let raw = &raw[1..];

        mem[orig..orig + raw.len()].clone_from_slice(&raw);
        // Add `HALT` at end of code and data
        // Prevents PC running through no-ops to the end of memory
        mem[orig + raw.len()] = 0xF025;

        Ok(RunEnvironment {
            state: RunState {
                mem: Box::new(mem),
                pc: orig as u16,
                reg: [0, 0, 0, 0, 0, 0, 0, 0xFDFF],
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
                if !Output::is_line_start() {
                    dprintln!(Always);
                }
                dprintln!(Sometimes, "Program counter at: 0x{:04x}", self.state.pc);
                match debugger.wait_for_action(&mut self.state) {
                    Action::Proceed => (),
                    Action::StopDebugger => {
                        self.debugger = None;
                        dprintln!(Always, "Stopping debugger.");
                        continue; // This is here for clarity
                    }
                    Action::ExitProgram => {
                        dprintln!(Always, "Exiting program.");
                        return;
                    }
                }
                // If still stuck on HALT
                // Never *execute* HALT while debugger is active
                // Wait for pc to change, such as `reset`, `exit`, or `quit`
                if RelevantInstr::try_from(self.state.mem[self.state.pc as usize])
                    == Ok(RelevantInstr::TrapHalt)
                {
                    continue;
                }
            }

            if self.state.pc == u16::MAX {
                break; // Halt was triggered
            }
            if self.state.pc >= 0xFE00 {
                exception!("entered protected memory area >= 0xFE00");
            }

            // if self.debugger.is_some() {
            // dprintln!("\x1b[2m-- executing one instruction!");
            // }

            let instr = self.state.mem[self.state.pc as usize];
            let opcode = (instr >> 12) as usize;
            // PC incremented before instruction is performed
            self.state.pc += 1;
            RunState::OP_TABLE[opcode](&mut self.state, instr);
        }

        if !Output::is_line_start() {
            println!();
        }
    }
}

impl RunState {
    const OP_TABLE: [fn(&mut RunState, u16); 16] = [
        Self::br,    // 0x0
        Self::add,   // 0x1
        Self::ld,    // 0x2
        Self::st,    // 0x3
        Self::jsr,   // 0x4
        Self::and,   // 0x5
        Self::ldr,   // 0x6
        Self::str,   // 0x7
        Self::rti,   // 0x8
        Self::not,   // 0x9
        Self::ldi,   // 0xA
        Self::sti,   // 0xB
        Self::jmp,   // 0xC
        Self::stack, // 0xD
        Self::lea,   // 0xE
        Self::trap,  // 0xF
    ];

    #[inline]
    pub(super) fn reg(&self, reg: u16) -> u16 {
        debug_assert!(reg < 8, "tried to access invalid register 'r{}'", reg);
        // SAFETY: Should only be indexed with values that are & 0b111
        unsafe { *self.reg.get_unchecked(reg as usize) }
    }
    #[inline]
    pub(super) fn reg_mut(&mut self, reg: u16) -> &mut u16 {
        debug_assert!(reg < 8, "tried to access invalid register 'r{}'", reg);
        // SAFETY: Should only be indexed with values that are & 0b111
        unsafe { self.reg.get_unchecked_mut(reg as usize) }
    }

    #[inline]
    pub(super) fn mem(&self, addr: u16) -> u16 {
        // SAFETY: memory fits any u16 index
        unsafe { *self.mem.get_unchecked(addr as usize) }
    }
    #[inline]
    pub(super) fn mem_mut(&mut self, addr: u16) -> &mut u16 {
        // SAFETY: memory fits any u16 index
        unsafe { self.mem.get_unchecked_mut(addr as usize) }
    }

    #[inline]
    pub(super) fn pc(&self) -> u16 {
        self.pc
    }

    #[inline]
    fn s_ext(mut val: u16, bits: u32) -> u16 {
        debug_assert!(bits > 0 && bits < 16);
        // Sign bit
        let sign = val & (1u16 << (bits - 1));
        // Bits lower than sign bit
        val &= (1u16 << bits) - 1;
        // Positive sign will always result in an extension of 0x0000
        // Negative sign will will set all upper bits and sign bit to 1
        let sign_extension = (!sign).wrapping_add(1); // sign * -1
        val | sign_extension
    }

    #[inline]
    fn set_flags(&mut self, val: u16) {
        self.flag = match (val as i16).cmp(&0) {
            Ordering::Less => RunFlag::N,
            Ordering::Equal => RunFlag::Z,
            Ordering::Greater => RunFlag::P,
        }
    }

    fn stack(&mut self, instr: u16) {
        if !env::is_stack_enabled() {
            eprintln!(
                "\
                You called a reserved instruction.\n\
                Note: Run with `LACE_STACK=1` to enable stack features.\n\
                Halting...\
                "
            );
            std::process::exit(1);
        }

        // Bit to determine call/ret or push/pop
        if instr & 0x0800 != 0 {
            // Call
            if instr & 0x0400 != 0 {
                self.push_val(self.pc);
                self.pc = self.pc.wrapping_add(Self::s_ext(instr, 10));
            }
            // Ret
            else {
                self.pc = self.pop_val();
            }
        } else {
            let reg = (instr >> 6) & 0b111;
            // Push
            if instr & 0x0400 != 0 {
                let val = self.reg(reg);
                self.push_val(val);
            }
            // Pop
            else {
                let val = self.pop_val();
                *self.reg_mut(reg) = val;
            }
        }
    }

    fn push_val(&mut self, val: u16) {
        debug_assert!(
            env::is_stack_enabled(),
            "caller should have ensured stack features are enabled",
        );
        // Decrement stack
        *self.reg_mut(7) -= 1;
        let sp = self.reg(7);
        // Save onto stack
        *self.mem_mut(sp) = val;
    }

    fn pop_val(&mut self) -> u16 {
        debug_assert!(
            env::is_stack_enabled(),
            "caller should have ensured stack features are enabled",
        );
        let sp = self.reg(7);
        let val = self.mem(sp);
        *self.reg_mut(7) += 1;
        val
    }

    fn add(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;

        let val1 = self.reg(sr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            self.reg(instr & 0b111)
        } else {
            // imm
            Self::s_ext(instr, 5)
        };
        let res = val1.wrapping_add(val2);
        self.set_flags(res);
        *self.reg_mut(dr) = res;
    }

    fn and(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;

        let val1 = self.reg(sr);
        // Check if imm
        let val2 = if instr & 0b100000 == 0 {
            // reg
            self.reg(instr & 0b111)
        } else {
            // imm
            Self::s_ext(instr, 5)
        };
        let res = val1 & val2;
        self.set_flags(res);
        *self.reg_mut(dr) = res;
    }

    fn br(&mut self, instr: u16) {
        let flag = (instr >> 9) & 0b111;
        if self.flag as u16 & flag != 0 {
            self.pc = self.pc.wrapping_add(Self::s_ext(instr, 9))
        }
    }

    fn jmp(&mut self, instr: u16) {
        let br = (instr >> 6) & 0b111;
        self.pc = self.reg(br)
    }

    fn jsr(&mut self, instr: u16) {
        *self.reg_mut(7) = self.pc;
        if instr & 0x800 == 0 {
            // reg
            let br = (instr >> 6) & 0b111;
            self.pc = self.reg(br)
        } else {
            // offs
            self.pc = self.pc.wrapping_add(Self::s_ext(instr, 11))
        }
    }

    fn ld(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let val = self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        *self.reg_mut(dr) = val;
        self.set_flags(val);
    }

    fn ldi(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let ptr = self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        let val = self.mem(ptr);
        *self.reg_mut(dr) = val;
        self.set_flags(val);
    }

    fn ldr(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let br = (instr >> 6) & 0b111;
        let ptr = self.reg(br);
        let val = self.mem(ptr.wrapping_add(Self::s_ext(instr, 6)));
        *self.reg_mut(dr) = val;
        self.set_flags(val);
    }

    fn lea(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let val = self.pc.wrapping_add(Self::s_ext(instr, 9));
        *self.reg_mut(dr) = val;
        self.set_flags(val);
    }

    fn not(&mut self, instr: u16) {
        let dr = (instr >> 9) & 0b111;
        let sr = (instr >> 6) & 0b111;
        let val = !self.reg(sr);
        *self.reg_mut(dr) = val;
        self.set_flags(val);
    }

    fn rti(&mut self, _instr: u16) {
        todo!("Please open an issue and I'll get RTI implemented in a jiffy :)")
    }

    fn st(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let val = *self.reg_mut(sr);
        *self.mem_mut(self.pc.wrapping_add(Self::s_ext(instr, 9))) = val;
    }

    fn sti(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let val = self.reg(sr);
        let ptr = self.mem(self.pc.wrapping_add(Self::s_ext(instr, 9)));
        *self.mem_mut(ptr) = val;
    }

    fn str(&mut self, instr: u16) {
        let sr = (instr >> 9) & 0b111;
        let br = (instr >> 6) & 0b111;
        let ptr = self.reg(br);
        let val = self.reg(sr);
        *self.mem_mut(ptr.wrapping_add(Self::s_ext(instr, 6))) = val;
    }

    fn trap(&mut self, instr: u16) {
        let trap_vect = instr & 0xFF;
        match trap_vect {
            // getc
            0x20 => {
                *self.reg_mut(0) = read_input() as u16;
            }
            // out
            0x21 => {
                let chr = (self.reg(0) & 0xFF) as u8 as char;
                print_char!(chr);
                stdout().flush().unwrap();
            }
            // puts
            0x22 => {
                // could probably rewrite with iterators but idk if worth
                for addr in self.reg(0).. {
                    let chr_raw = self.mem(addr);
                    let chr_ascii = (chr_raw & 0xFF) as u8 as char;
                    if chr_ascii == '\0' {
                        break;
                    }
                    print_char!(chr_ascii);
                }
                stdout().flush().unwrap();
            }
            // in
            0x23 => {
                let ch = read_input();
                *self.reg_mut(0) = ch as u16;
                print_char!(ch as char);
                stdout().flush().unwrap();
            }
            // putsp
            0x24 => {
                'string: for addr in self.reg(0).. {
                    let chr_raw = self.mem(addr);
                    for chr in [chr_raw >> 8, chr_raw & 0xFF] {
                        let chr_ascii = chr as u8 as char;
                        if chr_ascii == '\0' {
                            break 'string;
                        }
                        print_char!(chr_ascii);
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
                let val = self.reg(0);
                println!("{val}");
            }
            // reg
            0x27 => {
                if !Output::is_line_start() {
                    println!();
                }
                Output::Normal.print_registers(&self);
            }
            // unknown
            _ => exception!(
                "called a trap with an unknown vector of 0x{:02x}",
                trap_vect
            ),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn s_ext() {
        fn expect(input: u16, bits: u32, expected: u16) {
            let actual = RunState::s_ext(input, bits);
            if actual != expected {
                panic!(
                    "\ns_ext(0x{input:04x}, {bits})\n  Expected: 0x{expected:04x}\n    Actual: 0x{actual:04x}\n"
                );
            }
        }

        expect(0x0000, 15, 0x0000);
        expect(0x0000, 1, 0x0000);

        expect(0x0001, 15, 0x0001);
        expect(0x0001, 2, 0x0001);
        expect(0x0001, 1, 0xffff);

        expect(0x00ff, 15, 0x00ff);
        expect(0x00ff, 9, 0x00ff);
        expect(0x00ff, 8, 0xffff);
        expect(0x00ff, 7, 0xffff);

        expect(0x0100, 15, 0x0100);
        expect(0x0100, 10, 0x0100);
        expect(0x0100, 9, 0xff00);
        expect(0x0100, 8, 0x0000);

        expect(0x03ff, 15, 0x03ff);
        expect(0x03ff, 11, 0x03ff);
        expect(0x03ff, 10, 0xffff);
        expect(0x03ff, 7, 0xffff);

        expect(0x0400, 15, 0x0400);
        expect(0x0400, 12, 0x0400);
        expect(0x0400, 11, 0xfc00);
        expect(0x0400, 10, 0x0000);

        expect(0x07ff, 15, 0x07ff);
        expect(0x07ff, 12, 0x07ff);
        expect(0x07ff, 11, 0xffff);
        expect(0x07ff, 7, 0xffff);

        expect(0x0fff, 15, 0x0fff);
        expect(0x0fff, 13, 0x0fff);
        expect(0x0fff, 12, 0xffff);
        expect(0x0fff, 11, 0xffff);

        expect(0x1000, 15, 0x1000);
        expect(0x1000, 14, 0x1000);
        expect(0x1000, 13, 0xf000);
        expect(0x1000, 12, 0x0000);
        expect(0x1000, 11, 0x0000);

        expect(0x1fff, 15, 0x1fff);
        expect(0x1fff, 14, 0x1fff);
        expect(0x1fff, 13, 0xffff);

        expect(0x3000, 15, 0x3000);
        expect(0x3000, 14, 0xf000);
        expect(0x3000, 13, 0xf000);
        expect(0x3000, 12, 0x0000);

        expect(0x3fff, 15, 0x3fff);
        expect(0x3fff, 14, 0xffff);
        expect(0x3fff, 11, 0xffff);

        expect(0x7000, 15, 0xf000);
        expect(0x7000, 13, 0xf000);
        expect(0x7000, 12, 0x0000);

        expect(0x7fff, 15, 0xffff);
        expect(0x7fff, 11, 0xffff);

        expect(0xfffe, 15, 0xfffe);
        expect(0xfffe, 9, 0xfffe);
        expect(0xfffe, 2, 0xfffe);
        expect(0xfffe, 1, 0x0000);

        expect(0xffff, 15, 0xffff);
        expect(0xffff, 1, 0xffff);
    }
}
