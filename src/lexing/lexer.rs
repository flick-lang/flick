use crate::lexing::token::AssignmentSymbol::*;
use crate::lexing::token::ComparatorSymbol::*;
use crate::lexing::token::OperatorSymbol::*;
use crate::lexing::token::Token;
use crate::types::IntType;
use crate::Type;

/// A struct that takes source code and converts it tokens (see [Token])
///
/// Note that Lexer implements [Iterator][a], so using Lexer is simple.
///
/// First, create the lexer (`source_code` should be a `Vec<char>`)
///
/// ```
/// # use flick::Lexer;
/// # let source_code: Vec<_> = "foo(42)".to_string().chars().collect();
/// let mut lexer = Lexer::new(&source_code);
/// ```
///
/// Then, either collect the tokens:
///
/// ```
/// # use flick::Lexer;
/// # let source_code: Vec<_> = "foo(42)".to_string().chars().collect();
/// # let mut lexer = Lexer::new(&source_code);
/// let tokens: Vec<_> = lexer.collect();
/// ```
///
/// or iterate through them:
///
/// ```
/// # use flick::Lexer;
/// # let source_code: Vec<_> = "foo(42)".to_string().chars().collect();
/// # let mut lexer = Lexer::new(&source_code);
/// for token in lexer {
///     // ...
/// }
/// ```
///
/// [a]: #impl-Iterator-for-Lexer%3C'a%3E
pub struct Lexer<'a> {
    /// Source code slice
    source_code: &'a [char],

    /// Current location in source code
    ///
    /// In other words, the character at index `self.cursor` of `self.chars` hasn't
    /// been processed yet.
    cursor: usize,
}

impl<'a> Lexer<'a> {
    /// Returns a lexer instance ready to lex the provided `source_code`.
    pub fn new(source_code: &'a [char]) -> Self {
        Self {
            source_code,
            cursor: 0,
        }
    }

    /// Returns (and consumes) a reference to the next character in the source code.
    fn next_char(&mut self) -> Option<&char> {
        let char = self.source_code.get(self.cursor);
        self.cursor += 1;
        char
    }

    /// Returns a reference to the `n`-th character out of the remaining source code.
    ///
    /// Note: this function returns `None` if fewer than `n` characters remain.
    ///
    /// This function doesn't affect the internal state of the lexer (i.e., it doesn't consume
    /// any characters / it doesn't advance the internal cursor)
    fn peek_char(&self, n: usize) -> Option<&char> {
        self.source_code.get(self.cursor + (n - 1)) // n-1 to fix indexing
    }

    /// Advances the cursor past the next `n` characters without returning anything.
    fn skip_chars(&mut self, n: usize) {
        self.cursor += n;
    }

    /// Consumes source code characters until a token is formed; returns the token.
    ///
    /// Note: this function skips any initial whitespace (except for a newline, which is a
    /// token, namely [Token::Newline]).
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_non_newline_whitespace();

        // Figure out what type the next token is and call handling function
        let peeked_token = match (self.peek_char(1)?, self.peek_char(2)) {
            ('a'..='z' | 'A'..='Z' | '_', _) => return Some(self.read_word()),
            ('0'..='9', _) => return Some(self.read_int_literal()),
            ('/', Some('/')) => return Some(self.read_comment()),

            ('>', Some('=')) => Token::ComparatorSymbol(GreaterOrEqualTo),
            ('<', Some('=')) => Token::ComparatorSymbol(LessOrEqualTo),
            ('=', Some('=')) => Token::ComparatorSymbol(EqualTo),
            ('!', Some('=')) => Token::ComparatorSymbol(NotEqualTo),

            ('*', Some('=')) => Token::AssignmentSymbol(TimesEq),
            ('/', Some('=')) => Token::AssignmentSymbol(DivideEq),
            ('-', Some('=')) => Token::AssignmentSymbol(MinusEq),
            ('+', Some('=')) => Token::AssignmentSymbol(PlusEq),
            ('=', _) => Token::AssignmentSymbol(Eq),

            ('>', _) => Token::ComparatorSymbol(GreaterThan),
            ('<', _) => Token::ComparatorSymbol(LessThan),
            ('%', _) => Token::OperatorSymbol(Modulo),
            ('*', _) => Token::OperatorSymbol(Asterisk),
            ('/', _) => Token::OperatorSymbol(Slash),
            ('-', _) => Token::OperatorSymbol(Minus),
            ('+', _) => Token::OperatorSymbol(Plus),
            (',', _) => Token::Comma,
            ('(', _) => Token::LParen,
            (')', _) => Token::RParen,
            ('{', _) => Token::LSquirly,
            ('}', _) => Token::RSquirly,
            ('\n', _) => Token::Newline,

            (c, _) => panic!("unexpected character: {:?}", c),
        };

        self.skip_chars(peeked_token.get_char_count());
        Some(peeked_token)
    }

    /// Returns (and consumes) source code characters while `predicate` evaluates to `true`
    /// when applied to each character.
    fn take_chars_while(&mut self, predicate: impl Fn(&char) -> bool) -> String {
        let mut string = String::new();
        while let Some(c) = self.peek_char(1) {
            if predicate(c) {
                string.push(*self.next_char().unwrap());
            } else {
                break;
            }
        }
        string
    }

    /// Consumes source code characters while `predicate` evaluates to `true` when applied
    /// to each character.
    ///
    /// This function is equivalent to [Lexer::take_chars_while], except it doesn't return
    /// anything.
    fn skip_chars_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while let Some(c) = self.peek_char(1) {
            if predicate(c) {
                self.skip_chars(1);
            } else {
                break;
            }
        }
    }

    /// This functions skips non-newline whitespace using [Lexer::skip_chars_while] and an
    /// appropriate predicate.
    fn skip_non_newline_whitespace(&mut self) {
        self.skip_chars_while(|&c| c.is_whitespace() && c != '\n');
    }

    /// Consumes source code characters and returns the corresponding [Token], either a keyword
    /// (e.g., `while`) or an identifier (e.g., `foo`).
    ///
    /// # Assumptions:
    ///
    /// - The next source code character is one of `a-z`, `A-Z`, or `_`.
    fn read_word(&mut self) -> Token {
        let s = self.take_chars_while(|&c| c.is_ascii_alphanumeric() || c == '_');

        if (s.starts_with('u') || s.starts_with('i')) && s.len() > 1 && s.chars().skip(1).all(|c| c.is_ascii_digit()) {
            let num: String = s.chars().skip(1).collect();
            let width: u32 = num.parse().unwrap();
            match s.chars().next().unwrap() {
                'u' => return Token::Type(Type::Int(IntType { width, signed: false })),
                'i' => return Token::Type(Type::Int(IntType { width, signed: true })),
                _ => unreachable!(),
            }
        }

        match s.as_str() {
            "void" => Token::Type(Type::Void),
            "while" => Token::While,
            "pub" => Token::Pub,
            "fn" => Token::Fn,
            "ret" => Token::Ret,
            "if" => Token::If,
            "extern" => Token::Extern,
            "else" => Token::Else,
            _ => Token::Identifier(s),
        }
    }

    /// Consumes source code characters and returns the corresponding [Token::IntLiteral].
    ///
    /// # Assumptions:
    ///
    /// - The next source code character is a digit or a '-'
    fn read_int_literal(&mut self) -> Token {
        let number = self.take_chars_while(|&c| c.is_ascii_digit());
        Token::IntLiteral(number)
    }

    /// Consumes source code characters and returns the corresponding [Token::Comment] or
    /// [Token::Docstring].
    ///
    /// # Assumptions:
    ///
    /// - The next two/three source code characters are `//` or `///`.
    fn read_comment(&mut self) -> Token {
        match self.peek_char(3) {
            Some('/') => Token::Docstring(self.take_chars_while(|&c| c != '\n')),
            _ => Token::Comment(self.take_chars_while(|&c| c != '\n')),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// This function is an alias for [Lexer::next_token()].
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // todo make macro to avoid last three lines of boilerplate

    #[test]
    fn comments() {
        let source_code = "//    simple comment\n/// // / docstring";
        let expected_tokens = vec![
            Token::Comment("//    simple comment".to_string()),
            Token::Newline,
            Token::Docstring("/// // / docstring".to_string()),
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }

    #[test]
    fn variables() {
        let source_code = "i64 this_is_a_LONG_VARIABLE_NAME = 5\ni64 shortInt = 5";
        let expected_tokens = vec![
            Token::Type(Type::Int(IntType { signed: true, width: 64 })),
            Token::Identifier("this_is_a_LONG_VARIABLE_NAME".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::IntLiteral("5".to_string()),
            Token::Newline,
            Token::Type(Type::Int(IntType { signed: true, width: 64 })),
            Token::Identifier("shortInt".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::IntLiteral("5".to_string()),
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }

    #[test]
    fn while_loop() {
        let source_code = "while x <= 5 {}";
        let expected_tokens = vec![
            Token::While,
            Token::Identifier("x".to_string()),
            Token::ComparatorSymbol(LessOrEqualTo),
            Token::IntLiteral("5".to_string()),
            Token::LSquirly,
            Token::RSquirly,
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }
}
