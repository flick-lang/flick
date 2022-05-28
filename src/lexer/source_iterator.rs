use crate::lexer::location::Location;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug)]
pub struct SourceIterator<'a> {
    /// Location of character that self.next() will return next time it is called
    next_loc: Option<Location>,
    /// Location of what self.next() returned last time it was called
    cur_loc: Option<Location>,
    src: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> SourceIterator<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            next_loc: Some(Location::new(1, 1)),
            cur_loc: None,
            src: source.chars().enumerate().peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.src.peek().map(|&(_, c)| c)
    }

    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            if self.next().is_none() {
                break;
            }
        }
    }

    pub fn skip_while<P: Fn(char) -> bool>(&mut self, predicate: P) {
        loop {
            match self.peek() {
                Some(c) if predicate(c) => self.skip(1),
                _ => return,
            }
        }
    }

    pub fn count_while<P: Fn(char) -> bool>(&mut self, predicate: P) -> usize {
        let mut count = 0;
        loop {
            match self.peek() {
                Some(c) if predicate(c) => count += 1,
                _ => return count,
            }
            self.skip(1);
        }
    }

    pub fn take_while<P: Fn(char) -> bool>(&mut self, predicate: P) -> String {
        let mut contents = String::new();
        loop {
            match self.peek() {
                Some(c) if predicate(c) => contents.push(c),
                _ => return contents,
            }
            self.skip(1);
        }
    }

    /// Returns the next character of the iterator and steps the iterator. This functions also
    /// keeps track of line and column numbers.
    ///
    /// Assumption: Every function that steps the iterator does so through this function
    pub fn next(&mut self) -> Option<char> {
        let next = self.src.next().map(|(_, c)| c);

        self.cur_loc = self.next_loc;

        // Safe to unwrap self.next_loc when next is Some() because next_loc is only set to None
        // when self.src runs out of chars to yield
        match next {
            Some('\n') => {
                self.next_loc.as_mut().unwrap().line += 1;
                self.next_loc.as_mut().unwrap().col = 1;
            }
            Some(_) => self.next_loc.as_mut().unwrap().col += 1,
            None => self.next_loc = None,
        }

        next
    }

    pub fn loc(&self) -> Option<Location> {
        self.cur_loc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next() {
        let mut iter = SourceIterator::new("next");
        assert_eq!(iter.next(), Some('n'));
        assert_eq!(iter.next(), Some('e'));
        assert_eq!(iter.next(), Some('x'));
        assert_eq!(iter.next(), Some('t'));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn skip() {
        let mut iter = SourceIterator::new("ThomaxIsTheBest!");
        iter.skip(3);
        assert_eq!(iter.next(), Some('m'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('x'));
        iter.skip(100);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn count_while() {
        let mut iter = SourceIterator::new("////xy/");
        assert_eq!(iter.count_while(|c| c == '/'), 4);
        assert_eq!(iter.next(), Some('x'));
        assert_eq!(iter.next(), Some('y'));
        assert_eq!(iter.next(), Some('/'));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn skip_while() {
        let mut iter = SourceIterator::new("////xy/");
        iter.skip_while(|c| c == '/');

        assert_eq!(iter.next(), Some('x'));
        assert_eq!(iter.next(), Some('y'));
        assert_eq!(iter.next(), Some('/'));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn take_while() {
        let mut iter = SourceIterator::new("1234567890asdfghjkl");
        assert_eq!(iter.take_while(|c| c.is_ascii_digit()), "1234567890");
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('s'));
        assert_eq!(iter.next(), Some('d'));
        iter.skip(100);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn line_count() {
        let mut iter = SourceIterator::new("line 1\nline 2\nline 3");
        iter.skip(1);
        assert_eq!(iter.loc().map(|loc| loc.line), Some(1));
        iter.skip(7);
        assert_eq!(iter.loc().map(|loc| loc.line), Some(2));
        iter.skip(7);
        assert_eq!(iter.loc().map(|loc| loc.line), Some(3));
        iter.skip(5);
        assert_eq!(iter.loc().map(|loc| loc.line), Some(3));
        iter.skip(1);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.loc(), None);
    }

    #[test]
    fn col_count() {
        let mut iter = SourceIterator::new("123456789\n123456789");
        iter.skip(6);
        assert_eq!(iter.loc().map(|loc| loc.col), Some(6));
        iter.skip(6);
        assert_eq!(iter.loc().map(|loc| loc.col), Some(2));
        iter.skip(1);
        assert_eq!(iter.loc().map(|loc| loc.col), Some(3));
        iter.skip(100);
        assert_eq!(iter.next(), None);
    }
}
