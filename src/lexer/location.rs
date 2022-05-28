#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location {
    pub(crate) row: usize,
    pub(crate) col: usize,
}

impl Location {
    pub fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }
}

impl Into<Location> for (usize, usize) {
    fn into(self) -> Location {
        Location::new(self.0, self.1)
    }
}
