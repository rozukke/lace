// TODO(feat): Keep sorted!
#[derive(Debug)]
pub struct Breakpoints(Vec<Breakpoint>);

#[derive(Clone, Copy, Debug)]
pub struct Breakpoint {
    pub address: u16,
    pub is_predefined: bool,
}

impl Breakpoints {
    pub fn get(&self, address: u16) -> Option<Breakpoint> {
        for breakpoint in &self.0 {
            if breakpoint.address == address {
                return Some(*breakpoint);
            }
        }
        None
    }

    pub fn contains(&self, address: u16) -> bool {
        for breakpoint in &self.0 {
            if breakpoint.address == address {
                return true;
            }
        }
        false
    }

    pub fn insert(&mut self, breakpoint: Breakpoint) {
        self.0.push(breakpoint);
    }

    /// Removes every breakpoint with given address
    ///
    /// Returns whether any breakpoint was found with given address
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
