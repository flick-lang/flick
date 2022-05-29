pub use program::SourceFile;

#[warn(
    missing_debug_implementations,
    missing_copy_implementations,
    unused_import_braces,
    unused_lifetimes
)]
mod lexer;
mod program;

#[cfg(test)]
const TEST_FILE_PATH: &str = "/home/test.fl";
