mod lexer;
mod source_file;

pub use source_file::SourceFile;

#[cfg(test)]
const TEST_FILE_PATH: &str = "/home/test.fl";
