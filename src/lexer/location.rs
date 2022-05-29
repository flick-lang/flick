use std::path::Path;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location<'a> {
    pub(crate) file_path: &'a Path,
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl<'a> Location<'a> {
    pub fn new(file_path: &'a Path, line: usize, col: usize) -> Self {
        Self {
            file_path,
            line,
            col,
        }
    }
}
