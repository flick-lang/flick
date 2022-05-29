use std::fmt::{self, Display, Formatter, Write};

use colored::Colorize;

use crate::lexer::location::Location;

#[derive(Debug, PartialEq)]
pub struct Error<'a> {
    pub(crate) loc: Location<'a>,
    pub(crate) kind: ErrorKind,
}

impl<'a> Error<'a> {
    pub fn new(loc: Location<'a>, kind: ErrorKind) -> Self {
        Self { loc, kind }
    }

    /// Finds first newline before `index` (searching right to left) in the source.
    ///
    /// This function excludes `index` from the search.
    fn find_newline_before(&self, index: usize) -> Option<usize> {
        self.loc.source_file.source[..index].rfind('\n')
    }

    /// Finds first newline after `index` (searching left to right) in the source.
    ///
    /// This function excludes `index` from the search.
    fn find_newline_after(&self, index: usize) -> Option<usize> {
        self.loc.source_file.source[index + 1..]
            .find('\n')
            .map(|i| i + index + 1) // compensate for initial skip
    }

    fn get_context_lines(&self) -> (Option<&str>, &str, Option<&str>) {
        let source = &self.loc.source_file.source;

        // source = "a = 1\nb ≈ 2\nc = 3"
        //                  ^ ^  ^
        //                  0 1  2
        //
        // 0: cur_line_start (points at first char after newline)
        // 1: self.loc.source_index
        // 2: cur_line_end (points at newline)
        //
        // This way, cur_line is source[cur_line_start..cur_line_end]

        let cur_line_start = self
            .find_newline_before(self.loc.source_index)
            .map(|i| i + 1); // don't include the \n in cur_line
        let cur_line_end = self.find_newline_after(self.loc.source_index);
        let cur_line = &source[cur_line_start.unwrap_or(0)..cur_line_end.unwrap_or(source.len())];

        let prev_line = match cur_line_start {
            Some(i) => {
                let prev_line_start = self
                    .find_newline_before(i - 1) // i-1 is the index of the \n right before cur_line
                    .map(|newline| newline + 1) // don't include the \n in prev_line
                    .unwrap_or(0);
                Some(&source[prev_line_start..i - 1]) // don't include \n right before cur_line
            }
            None => None,
        };

        let next_line = match cur_line_end {
            Some(i) => {
                let next_line_end = self
                    .find_newline_after(i) // i is the index of the \n right after cur_line
                    .unwrap_or(source.len());
                Some(&source[i + 1..next_line_end]) // don't include the \n right after cur_line
            }
            None => None,
        };

        (prev_line, cur_line, next_line)
    }

    fn write_context_line(
        mut s: impl Write,
        line: &str,
        line_num: usize,
        line_num_width: usize,
    ) -> fmt::Result {
        writeln!(
            s,
            " {:0>width$} │ {}",
            line_num,
            line,
            width = line_num_width
        )
    }
}

impl<'a> std::error::Error for Error<'a> {}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        writeln!(
            s,
            "{}: {}",
            "error".red().bold(),
            self.kind.to_string().bold()
        )?;

        let (prev, cur, next) = self.get_context_lines();

        let line_num_width = match next {
            Some(_) => (self.loc.line + 1).to_string().len(),
            None => self.loc.line.to_string().len(),
        };

        if let Some(line) = prev {
            Self::write_context_line(&mut s, line, self.loc.line - 1, line_num_width)?;
        }

        Self::write_context_line(&mut s, cur, self.loc.line, line_num_width)?;

        if let Some(line) = next {
            Self::write_context_line(&mut s, line, self.loc.line + 1, line_num_width)?;
        }

        f.write_str(&s)
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    BadUnicodeEscape(String),
    InvalidCharInEscape(char),
    InvalidFloat(String),
    TruncatedEscapeSequence,
    UnknownEscape(char),
    UnknownStartOfToken(char),
    UnterminatedStr,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BadUnicodeEscape(s) => {
                write!(f, "bad unicode escape sequence '{}'", s)
            }
            Self::InvalidCharInEscape(c) => {
                write!(f, "invalid character in escape sequence: '{}'", c)
            }
            Self::InvalidFloat(float) => write!(f, "invalid float literal '{}'", float),
            Self::TruncatedEscapeSequence => write!(f, "escape sequence is too short"),
            Self::UnknownEscape(c) => write!(f, "unknown escape '{}'", c),
            Self::UnknownStartOfToken(c) => {
                write!(f, "unknown start of token (U+{:0>4X})", *c as u32)
            }
            Self::UnterminatedStr => write!(f, "unterminated string literal"),
        }
    }
}

impl std::error::Error for ErrorKind {}
