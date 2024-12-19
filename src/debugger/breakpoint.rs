/// Wrapper for list of [`Breakpoint`]s.
///
/// Could be another collection, but [`Vec`] was used for simplicity.
///
/// List must remain sorted, and 2 breakpoints cannot have the same address.
#[derive(Debug)]
pub struct Breakpoints(Vec<Breakpoint>);

/// A [`Breakpoint`] is just an address, and a flag for whether it was 'predefined'.
///
/// Predefined here meaning it was registered in the assembly code, with the `.BREAK` directive,
/// as opposed to being registered with a debugger command (`break add`).
#[derive(Clone, Copy, Debug)]
pub struct Breakpoint {
    pub address: u16,
    pub is_predefined: bool,
}

impl Breakpoints {
    /// Get the [`Breakpoint`] with the given address.
    ///
    /// Returns `None` if no breakpoint exists.
    pub fn get(&self, address: u16) -> Option<Breakpoint> {
        for breakpoint in &self.0 {
            if breakpoint.address == address {
                return Some(*breakpoint);
            }
        }
        None
    }

    /// Insert a new breakpoint, keeping list sorted.
    ///
    /// Returns `true` if breakpoint already exists with that address (new breakpoint will not be
    /// inserted).
    pub fn insert(&mut self, breakpoint: Breakpoint) -> bool {
        let mut index = self.len();
        for (i, other) in self.iter().enumerate() {
            if other.address == breakpoint.address {
                return true;
            }
            if other.address >= breakpoint.address {
                index = i;
                break;
            }
        }
        self.0.insert(index, breakpoint);
        return false;
    }

    /// Removes every breakpoint with given address, keeping list sorted.
    ///
    /// Returns whether any breakpoint was found with given address.
    pub fn remove(&mut self, address: u16) -> bool {
        let initial_len = self.0.len();
        self.0.retain(|breakpoint| breakpoint.address != address);
        initial_len != self.0.len()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &Breakpoint> {
        self.0.iter()
    }
}

impl From<Vec<Breakpoint>> for Breakpoints {
    fn from(vec: Vec<Breakpoint>) -> Self {
        Self(vec)
    }
}

impl<'a> IntoIterator for &'a Breakpoints {
    type Item = &'a Breakpoint;
    type IntoIter = std::slice::Iter<'a, Breakpoint>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
