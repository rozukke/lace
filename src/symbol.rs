use std::cell::RefCell;

use fxhash::FxBuildHasher;
use indexmap::IndexMap;

// Symbol table of symbol -> memory address (line number)
type FxMap<K, V> = IndexMap<K, V, FxBuildHasher>;

thread_local! {
    pub static SYMBOL_TABLE: RefCell<FxMap<String, u16>> = RefCell::new(IndexMap::with_hasher(FxBuildHasher::default()));
}

pub fn with_symbol_table<R, F>(f: F) -> R
where
    F: FnOnce(&mut FxMap<String, u16>) -> R,
{
    SYMBOL_TABLE.with_borrow_mut(f)
}

/// Reference to symbol table index
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Symbol(usize);

impl From<usize> for Symbol {
    fn from(value: usize) -> Self {
        Symbol { 0: value }
    }
}

/// Location within source
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    offs: ByteOffs,
    len: usize,
}

impl Span {
    pub fn new(offs: ByteOffs, len: usize) -> Self {
        Span { offs, len }
    }
}

/// Represents the CPU registers.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    /// Generally used as the stack pointer.
    R7,
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TrapKind {
    Trap(u16),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DirKind {
    Orig,
}

/// Newtype representing an address inside the LC3 memory.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Addr(u16);

/// Newtype representing an offset from a particular address.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ByteOffs(u16);

/// Label used to refer to specific memory addresses
/// TODO: optimize later
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Label(String);
