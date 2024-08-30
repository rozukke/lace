use std::{
    cell::RefCell,
    ops::{Bound, Range, RangeBounds},
    slice::SliceIndex,
    str::FromStr,
    usize,
};

use fxhash::FxBuildHasher;
use indexmap::IndexMap;
use miette::{bail, miette, Result, Severity, SourceSpan};

// Symbol table of symbol -> memory address (line number)
type FxMap<K, V> = IndexMap<K, V, FxBuildHasher>;

thread_local! {
    pub static SYMBOL_TABLE: RefCell<FxMap<String, u16>> = RefCell::new(IndexMap::with_hasher(FxBuildHasher::default()));
}

/// Access to symbol table via closure
pub fn with_symbol_table<R, F>(f: F) -> R
where
    F: FnOnce(&mut FxMap<String, u16>) -> R,
{
    SYMBOL_TABLE.with_borrow_mut(f)
}

/// Line number of referenced label
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Label(Option<u16>);

impl Label {
    /// Called on prefix labels. Errors on duplicates.
    pub fn insert(label: &str, line: u16) -> Result<Self> {
        with_symbol_table(|sym| {
            // Some is returned if the label already exists
            if let Some(_) = sym.insert(label.to_string(), line) {
                Err(miette!("Label exists"))
            } else {
                Ok(Label { 0: Some(line) })
            }
        })
    }

    /// Used on non-prefix labels to give them a discrete line number reference
    pub fn try_fill(label: &str) -> Option<Self> {
        with_symbol_table(|sym| {
            // Fill with existing label value
            if let Some(val) = sym.get(label) {
                Some(Label { 0: Some(*val) })
            } else {
                None
            }
        })
    }

    /// Used when all prefix labels are guaranteed to exist in table
    pub fn fill(label: &str) -> Result<Self> {
        with_symbol_table(|sym| {
            // Check if not None, Err otherwise
            if let Some(val) = sym.get(label) {
                Ok(Label { 0: Some(*val) })
            } else {
                Err(miette!("Label not found"))
            }
        })
    }

    pub fn is_unfilled(&self) -> bool {
        match self.0 {
            Some(_) => false,
            None => true,
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
