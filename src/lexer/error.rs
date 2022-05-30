use std::fmt::{self, Display, Formatter, Write};

use colored::{control::SHOULD_COLORIZE, Colorize};

use crate::lexer::location::Location;

#[derive(Debug, PartialEq)]
pub struct Error<'a> {
    /// loc stores the location of the last character in the error
    pub(crate) loc: Location<'a>,
    pub(crate) kind: ErrorKind,
    pub(crate) problem: String,
}

impl<'a> std::error::Error for Error<'a> {}

impl<'a> Error<'a> {
    pub fn new(loc: Location<'a>, kind: ErrorKind, problem: impl Into<String>) -> Self {
        Self {
            loc,
            kind,
            problem: problem.into(),
        }
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

    fn write_squiggle(&self, mut s: impl Write, line_num_width: usize) -> fmt::Result {
        let offset = " ".repeat(line_num_width + 2);
        let padding = " ".repeat(1 + self.loc.col - self.problem.len());
        let squiggle = "^".repeat(self.problem.len());
        writeln!(s, "{}│{}{}", offset, padding, squiggle.red())
    }

    fn highlight_error(&self, line: &str) -> String {
        let end = self.loc.col;
        let start = end - self.problem.len();
        format!(
            "{}{}{}",
            &line[..start],
            &line[start..end].red().bold(),
            &line[end..]
        )
    }

    fn write_error_line(&self, mut s: impl Write) -> fmt::Result {
        write!(s, "{}: ", "error".red().bold())?;
        match self.kind {
            ErrorKind::InvalidCharInEscape => {
                writeln!(
                    s,
                    "invalid character in escape sequence: '{}'",
                    self.problem
                )
            }
            ErrorKind::InvalidFloat => writeln!(s, "invalid float literal: '{}'", self.problem),
            ErrorKind::TruncatedEscapeSequence => {
                writeln!(s, "escape sequence is too short: '{}'", self.problem)
            }
            ErrorKind::UnknownEscape => writeln!(s, r"unknown escape: '{}'", self.problem),
            ErrorKind::InvalidStartOfToken => {
                writeln!(
                    s,
                    "invalid start of token: '{}' ({})",
                    self.problem,
                    self.problem.escape_unicode().collect::<String>()
                )
            }
            ErrorKind::UnterminatedStr => writeln!(s, "unterminated string literal"),
        }
    }

    pub(crate) fn prepend(mut self, prefix: impl Into<String>) -> Self {
        self.problem = prefix.into() + &self.problem;
        self
    }
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        self.write_error_line(&mut s)?;

        let (prev, cur, next) = self.get_context_lines();

        let line_num_width = match next {
            Some(_) => (self.loc.line + 1).to_string().len(),
            None => self.loc.line.to_string().len(),
        };

        if let Some(line) = prev {
            Self::write_context_line(&mut s, line, self.loc.line - 1, line_num_width)?;
        }

        Self::write_context_line(
            &mut s,
            &self.highlight_error(cur),
            self.loc.line,
            line_num_width,
        )?;

        if !SHOULD_COLORIZE.should_colorize() {
            self.write_squiggle(&mut s, line_num_width)?; // if terminal doesn't support colors
        }

        if let Some(line) = next {
            Self::write_context_line(&mut s, line, self.loc.line + 1, line_num_width)?;
        }

        write!(
            s,
            "{}:{}:{}",
            self.loc.source_file.file_path.display().to_string().bold(),
            self.loc.line.to_string().bold(),
            (self.loc.col - self.problem.len() + 1).to_string().bold()
        )?;

        f.write_str(&s)
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    InvalidCharInEscape,
    InvalidFloat,
    TruncatedEscapeSequence,
    UnknownEscape,
    InvalidStartOfToken,
    UnterminatedStr,
}
