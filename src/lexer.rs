use crate::token::OperatorSymbol::*;
use crate::token::{Token, Type};

pub struct Lexer<'a> {
    chars: &'a [char],
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a [char]) -> Self {
        Self { chars, cursor: 0 }
    }

    fn next_char(&mut self) -> Option<&char> {
        let char = self.chars.get(self.cursor);
        self.cursor += 1;
        char
    }

    /// Returns a reference to the next() value without advancing the cursor.
    fn peek_char(&self, n: usize) -> Option<&char> {
        self.chars.get(self.cursor + (n - 1)) // n-1 to fix indexing
    }

    fn skip_chars(&mut self, n: usize) {
        self.cursor += n;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_non_newline_whitespace();

        // Figure out what type the next token is and call handling function
        let peeked_token = match (self.peek_char(1)?, self.peek_char(2)) {
            ('a'..='z' | 'A'..='Z' | '_', _) => return Some(self.read_word()),
            ('0'..='9', _) => return Some(self.read_i64_literal()),
            ('/', Some('/')) => return Some(self.read_comment()),

            ('>', Some('=')) => Token::OperatorSymbol(GreaterOrEqualTo),
            ('<', Some('=')) => Token::OperatorSymbol(LessOrEqualTo),
            ('=', Some('=')) => Token::OperatorSymbol(EqualTo),
            ('!', Some('=')) => Token::OperatorSymbol(NotEqualTo),

            ('*', Some('=')) => Token::OperatorSymbol(TimesEq),
            ('/', Some('=')) => Token::OperatorSymbol(DivideEq),
            ('-', Some('=')) => Token::OperatorSymbol(MinusEq),
            ('+', Some('=')) => Token::OperatorSymbol(PlusEq),

            ('>', _) => Token::OperatorSymbol(GreaterThan),
            ('<', _) => Token::OperatorSymbol(LessThan),
            ('=', _) => Token::OperatorSymbol(Assign),
            ('*', _) => Token::OperatorSymbol(Asterisk),
            ('/', _) => Token::OperatorSymbol(Slash),
            ('-', _) => Token::OperatorSymbol(Minus),
            ('+', _) => Token::OperatorSymbol(Plus),
            (',', _) => Token::Comma,
            (':', _) => Token::Colon,
            ('(', _) => Token::LParen,
            (')', _) => Token::RParen,
            ('[', _) => Token::LSquare,
            (']', _) => Token::RSquare,
            ('{', _) => Token::LSquirly,
            ('}', _) => Token::RSquirly,
            ('\n', _) => Token::Newline,

            (c, _) => panic!("unexpected character: {:?}", c),
        };

        self.skip_chars(peeked_token.get_char_count());
        Some(peeked_token)
    }

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

    fn skip_chars_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while let Some(c) = self.peek_char(1) {
            if predicate(c) {
                self.skip_chars(1);
            } else {
                break;
            }
        }
    }

    fn skip_non_newline_whitespace(&mut self) {
        self.skip_chars_while(|&c| c.is_whitespace() && c != '\n');
    }

    /// Assuming lexer peeked 'a'-'z', 'A'-'Z', or '_'.
    fn read_word(&mut self) -> Token {
        let s = self.take_chars_while(|&c| c.is_ascii_alphanumeric() || c == '_');
        match s.as_str() {
            "i64" => Token::Type(Type::I64),
            "void" => Token::Type(Type::Void),
            "while" => Token::While,
            "fn" => Token::Fn,
            _ => Token::Identifier(s),
        }
    }

    /// Assuming lexer peeked a digit.
    fn read_i64_literal(&mut self) -> Token {
        let number = self.take_chars_while(|&c| c.is_ascii_digit());
        Token::I64Literal(number.parse().unwrap())
        // TODO: Handle error parsing int! it could be too big for i64
    }

    /// Assuming lexer peeked a '#'.
    fn read_comment(&mut self) -> Token {
        match self.peek_char(3) {
            Some('/') => Token::Docstring(self.take_chars_while(|&c| c != '\n')),
            _ => Token::Comment(self.take_chars_while(|&c| c != '\n')),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Lexes the next `Token` and returns it.
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::{OperatorSymbol, Token, Type};
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
            Token::Type(Type::I64),
            Token::Identifier("this_is_a_LONG_VARIABLE_NAME".to_string()),
            Token::OperatorSymbol(OperatorSymbol::Assign),
            Token::I64Literal(5),
            Token::Newline,
            Token::Type(Type::I64),
            Token::Identifier("shortInt".to_string()),
            Token::OperatorSymbol(OperatorSymbol::Assign),
            Token::I64Literal(5),
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
            Token::OperatorSymbol(OperatorSymbol::LessOrEqualTo),
            Token::I64Literal(5),
            Token::LSquirly,
            Token::RSquirly,
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }
}
