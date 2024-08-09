/// Position relative to start of source.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Idx(pub u32);

/// Holds a view into a source.
#[derive(Clone, Copy, PartialEq, Eq, Default, Hash, Debug)]
pub struct Span {
    start: Idx,
    len: u16,
}

impl Span {
    pub fn new(start: Idx, len: u16) -> Self {
        Span { start, len }
    }

    pub fn as_range(&self) -> std::ops::Range<usize> {
        let start = self.start.0 as usize;
        let end = start + self.len as usize;
        start..end
    }
}
