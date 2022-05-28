#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl Location {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl Into<Location> for (usize, usize) {
    fn into(self) -> Location {
        Location::new(self.0, self.1)
    }
}
