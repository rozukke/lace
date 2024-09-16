use std::{cell::RefCell, ops::Range, str::FromStr};

use fxhash::FxHashMap;
use miette::{miette, Result, SourceSpan};

thread_local! {
    pub static SYMBOL_TABLE: RefCell<FxHashMap<String, u16>> = RefCell::new(FxHashMap::default());
}

pub fn reset_state() {
    with_symbol_table(|sym| sym.clear());
}

/// Access to symbol table via closure
pub fn with_symbol_table<R, F>(f: F) -> R
where
    F: FnOnce(&mut FxHashMap<String, u16>) -> R,
{
    SYMBOL_TABLE.with_borrow_mut(f)
}

/// This is not allowed to be cloned to avoid double frees.
pub struct StaticSource {
    src: *mut String,
}

impl StaticSource {
    pub fn new(src: String) -> Self {
        let raw_ptr = Box::into_raw(Box::new(src));
        Self { src: raw_ptr }
    }

    pub fn src(&self) -> &'static str {
        unsafe { &*self.src }
    }
}

impl Drop for StaticSource {
    fn drop(&mut self) {
        unsafe { drop(Box::from_raw(self.src)) }
    }
}

/// Line number of referenced label
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Label {
    Ref(u16),
    Unfilled(String),
}

impl Label {
    /// Called on prefix labels. Errors on duplicates.
    pub fn insert(label: &str, line: u16) -> Result<()> {
        with_symbol_table(|sym| {
            // Some is returned if the label already exists
            if sym.insert(label.to_string(), line).is_some() {
                Err(miette!("Label exists"))
            } else {
                Ok(())
            }
        })
    }

    /// Used on non-prefix labels to give them a discrete line number reference
    pub fn try_fill(label: &str) -> Self {
        with_symbol_table(|sym| {
            // Fill with existing label value
            if let Some(val) = sym.get(label) {
                Label::Ref(*val)
            } else {
                Label::Unfilled(label.to_string())
            }
        })
    }

    /// Used when all prefix labels are guaranteed to exist in table
    pub fn filled(self) -> Result<Self> {
        with_symbol_table(|sym| match &self {
            Self::Unfilled(label) => {
                if let Some(line) = sym.get(label.as_str()) {
                    Ok(Self::Ref(*line))
                } else {
                    Err(miette!("Label not found"))
                }
            }
            Self::Ref(_) => Ok(self),
        })
    }

    /// For comparison in tests
    pub fn empty(val: &str) -> Self {
        Label::Unfilled(val.to_string())
    }

    /// Function for testing purposes only
    pub fn dummy(val: u16) -> Self {
        Label::Ref(val)
    }

    /// Check if label is filled
    pub fn is_unfilled(&self) -> bool {
        match self {
            Label::Unfilled(_) => false,
            Label::Ref(_) => true,
        }
    }
}

/// Location within source str
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    offs: SrcOffset,
    len: usize,
}

impl Span {
    pub fn new(offs: SrcOffset, len: usize) -> Self {
        Span { offs, len }
    }

    /// Non-source span
    pub fn dummy() -> Self {
        Span {
            offs: SrcOffset(0),
            len: 0,
        }
    }

    /// Returns a range that can be used to index the source
    pub fn as_range(&self) -> Range<usize> {
        self.offs()..self.end()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn offs(&self) -> usize {
        self.offs.0
    }

    pub fn end(&self) -> usize {
        self.offs.0 + self.len
    }
}

// Used for miette conversion
impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.offs().into(), value.len())
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            offs: SrcOffset(value.start),
            len: value.end - value.start,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.offs()..value.end()
    }
}

/// Represents the CPU registers.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Register {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    /// Generally used as the stack pointer.
    R7,
}

impl FromStr for Register {
    type Err = ();

    // Does not fail in this codebase.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Register::R0),
            "1" => Ok(Register::R1),
            "2" => Ok(Register::R2),
            "3" => Ok(Register::R3),
            "4" => Ok(Register::R4),
            "5" => Ok(Register::R5),
            "6" => Ok(Register::R6),
            "7" => Ok(Register::R7),
            _ => Err(()),
        }
    }
}

/// Set by a subset of instructions, representing whether the result was negative, zero, or positive.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Flag {
    /// -
    N,
    /// 0
    Z,
    /// +
    P,
    /// <= 0
    Nz,
    /// >= 0
    Zp,
    /// != 0
    Np,
    /// Unconditional
    Nzp,
}

impl Flag {
    pub fn bits(&self) -> u16 {
        match self {
            Flag::N => 0b100,
            Flag::Z => 0b010,
            Flag::P => 0b001,
            Flag::Nz => 0b110,
            Flag::Zp => 0b011,
            Flag::Np => 0b101,
            Flag::Nzp => 0b111,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InstrKind {
    Add,
    And,
    Br(Flag),
    Jmp,
    Jsr,
    Jsrr,
    Ld,
    Ldi,
    Ldr,
    Lea,
    Not,
    Ret,
    Rti,
    St,
    Sti,
    Str,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TrapKind {
    Generic,
    Halt,
    Putsp,
    In,
    Puts,
    Out,
    Getc,
    Putn,
    Reg,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DirKind {
    Orig,
    End,
    Stringz,
    Blkw,
    Fill,
}

/// Newtype representing an offset from a particular address.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct LineOffs(u16);

/// Used to refer to offsets from the start of a source file.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SrcOffset(pub usize);
