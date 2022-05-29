use crate::SourceFile;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Location<'a> {
    pub(crate) source_file: &'a SourceFile,
    pub(crate) source_index: usize,
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl<'a> Location<'a> {
    pub fn new(source_file: &'a SourceFile, source_index: usize, line: usize, col: usize) -> Self {
        Self {
            source_file,
            source_index,
            line,
            col,
        }
    }
}
