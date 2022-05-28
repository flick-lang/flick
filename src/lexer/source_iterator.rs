use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug)]
pub struct SourceIterator<'a> {
    src: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> SourceIterator<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            src: source.chars().enumerate().peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.src.peek().map(|&(_, c)| c)
    }

    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            if self.src.next().is_none() {
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

    pub fn next(&mut self) -> Option<char> {
        self.src.next().map(|(_, c)| c)
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
}
