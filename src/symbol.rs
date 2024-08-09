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
}

/// Newtype representing an address inside the LC3 memory.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Addr(u16);

/// Newtype representing an offset from a particular address.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ByteOffs(u16);

/// Label used to refer to specific memory addresses
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Label<'a>(&'a str);
