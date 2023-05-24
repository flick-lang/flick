use std::iter::Peekable;
use std::str::Chars;

use crate::token::Token;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            chars: source_code.chars().peekable(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_non_newline_whitespace();

        // Figure out what type the next token is and call handling function
        let token = match self.chars.peek()? {
            'a'..='z' | 'A'..='Z' | '_' => self.read_word(),
            '0'..='9' => self.read_usize_literal(),
            '#' => self.read_comment(),
            '=' | '<' | '>' | '*' | '%' | '/' | '+' | '-' => self.read_operator(),
            ':' | '(' | ')' | '{' | '}' | '\n' => self.read_punctuation(),
            _ => unreachable!(),
        };

        Some(token)
    }

    fn take_chars_while(&mut self, predicate: impl Fn(&char) -> bool) -> String {
        let mut string = String::new();
        while let Some(c) = self.chars.peek() {
            if predicate(c) {
                string.push(self.chars.next().unwrap());
            } else {
                break
            }
        }
        string
    }

    fn skip_chars_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while let Some(c) = self.chars.peek() {
            if predicate(c) {
                self.chars.next();
            } else {
                break
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
            "var" => Token::Var,
            "int" => Token::Int,
            "while" => Token::While,
            _ => Token::Identifier(s),
        }
    }

    /// Assuming lexer peeked a digit.
    fn read_usize_literal(&mut self) -> Token {
        let number = self.take_chars_while(|&c| c.is_ascii_digit());
        Token::Usize(number.parse().unwrap())
    }

    /// Assuming lexer peeked a '#'.
    fn read_comment(&mut self) -> Token {
        self.skip_chars_while(|&c| c == '#');
        self.skip_non_newline_whitespace();

        Token::Comment(self.take_chars_while(|&c| c != '\n'))
    }

    /// Assuming lexer peeked a character like '*' or '>' that might be proceeded
    /// by an '='.
    fn read_operator(&mut self) -> Token {
        let mut operator = String::new();
        operator.push(self.chars.next().unwrap());

        if let Some('=') = self.chars.peek() {
            operator.push(self.chars.next().unwrap());
        }

        match operator.as_str() {
            ">" => Token::GreaterThan,
            "<" => Token::LessThan,
            "=" => Token::Assign,
            "*" => Token::Asterisk,
            "/" => Token::Slash,
            "-" => Token::Minus,
            "+" => Token::Plus,

            ">=" => Token::GreaterOrEqualTo,
            "<=" => Token::LessOrEqualTo,
            "==" => Token::EqualTo,
            "*=" => Token::TimesEq,
            "/=" => Token::DivideEq,
            "-=" => Token::MinusEq,
            "+=" => Token::PlusEq,

            _ => unreachable!(),
        }
    }

    fn read_punctuation(&mut self) -> Token {
        match self.chars.next().unwrap() {
            ':' => Token::Colon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LSquirly,
            '}' => Token::RSquirly,
            '\n' => Token::Newline,
            _ => unreachable!(),
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
    use crate::token::Token;
    use crate::lexer::Lexer;
    // todo make macro to avoid last three lines of boilerplate

    #[test]
    fn comments() {
        let source_code = "#    simple comment\n######   ## complex comment";
        let expected_tokens = vec![
            Token::Comment("simple comment".to_string()),
            Token::Newline,
            Token::Comment("## complex comment".to_string()),
        ];

        let lexer = Lexer::new(source_code);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }

    #[test]
    fn variables() {
        let source_code = "var this_is_a_LONG_VARIABLE_NAME: int = 5\nvar shortInt: int = 5";
        let expected_tokens = vec![
            Token::Var,
            Token::Identifier("this_is_a_LONG_VARIABLE_NAME".to_string()),
            Token::Colon,
            Token::Int,
            Token::Assign,
            Token::Usize(5),
            Token::Newline,
            Token::Var,
            Token::Identifier("shortInt".to_string()),
            Token::Colon,
            Token::Int,
            Token::Assign,
            Token::Usize(5),
        ];

        let lexer = Lexer::new(source_code);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }

    #[test]
    fn while_loop() {
        let source_code = "while x <= 5 {}";
        let expected_tokens = vec![
            Token::While,
            Token::Identifier("x".to_string()),
            Token::LessOrEqualTo,
            Token::Usize(5),
            Token::LSquirly,
            Token::RSquirly,
        ];

        let lexer = Lexer::new(source_code);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }
}
