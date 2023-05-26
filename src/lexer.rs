use crate::token::OperatorSymbol::*;
use crate::token::{OperatorSymbol, Token, VarType};

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

    fn skip_char(&mut self) {
        self.cursor += 1;
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_non_newline_whitespace();

        // Figure out what type the next token is and call handling function
        let token = match self.peek_char(1)? {
            'a'..='z' | 'A'..='Z' | '_' => self.read_word(),
            '0'..='9' => self.read_int_literal(),
            '#' => self.read_comment(),
            '=' | '<' | '>' | '*' | '%' | '/' | '+' | '-' => self.read_operator(),
            ',' | ':' | '(' | ')' | '{' | '}' | '\n' => self.read_punctuation(),
            _ => unreachable!(),
        };

        Some(token)
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
                self.skip_char();
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
            "var" => Token::Var,
            "int" => Token::VarType(VarType::Int),
            "while" => Token::While,
            _ => Token::Identifier(s),
        }
    }

    /// Assuming lexer peeked a digit.
    fn read_int_literal(&mut self) -> Token {
        let number = self.take_chars_while(|&c| c.is_ascii_digit());
        Token::IntLiteral(number.parse().unwrap())
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
        operator.push(*self.next_char().unwrap());

        if let Some('=') = self.peek_char(1) {
            operator.push(*self.next_char().unwrap());
        }

        match operator.as_str() {
            ">" => Token::OperatorSymbol(GreaterThan),
            "<" => Token::OperatorSymbol(LessThan),
            "=" => Token::OperatorSymbol(Assign),
            "*" => Token::OperatorSymbol(Asterisk),
            "/" => Token::OperatorSymbol(Slash),
            "-" => Token::OperatorSymbol(Minus),
            "+" => Token::OperatorSymbol(Plus),

            ">=" => Token::OperatorSymbol(GreaterOrEqualTo),
            "<=" => Token::OperatorSymbol(LessOrEqualTo),
            "==" => Token::OperatorSymbol(EqualTo),
            "!=" => Token::OperatorSymbol(NotEqualTo),
            "*=" => Token::OperatorSymbol(TimesEq),
            "/=" => Token::OperatorSymbol(DivideEq),
            "-=" => Token::OperatorSymbol(MinusEq),
            "+=" => Token::OperatorSymbol(PlusEq),

            _ => unreachable!(),
        }
    }

    fn read_punctuation(&mut self) -> Token {
        match self.next_char().unwrap() {
            ',' => Token::Comma,
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
    use crate::lexer::Lexer;
    use crate::token::{OperatorSymbol, Token, VarType};
    // todo make macro to avoid last three lines of boilerplate

    #[test]
    fn comments() {
        let source_code = "#    simple comment\n######   ## complex comment";
        let expected_tokens = vec![
            Token::Comment("simple comment".to_string()),
            Token::Newline,
            Token::Comment("## complex comment".to_string()),
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
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
            Token::VarType(VarType::Int),
            Token::OperatorSymbol(OperatorSymbol::Assign),
            Token::IntLiteral(5),
            Token::Newline,
            Token::Var,
            Token::Identifier("shortInt".to_string()),
            Token::Colon,
            Token::VarType(VarType::Int),
            Token::OperatorSymbol(OperatorSymbol::Assign),
            Token::IntLiteral(5),
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
            Token::IntLiteral(5),
            Token::LSquirly,
            Token::RSquirly,
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let received_tokens: Vec<_> = lexer.collect();

        assert_eq!(received_tokens, expected_tokens);
    }
}
