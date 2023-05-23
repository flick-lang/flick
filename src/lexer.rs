use std::iter::Peekable;
use std::str::Chars;

use crate::token::Token;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            chars: source_code.chars().peekable()
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

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

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_whitespace() && *c != '\n' {
                self.chars.next();
            } else {
                break
            }
        }
    }

    fn read_word(&mut self) -> Token {
        let mut s = String::new();
        while let Some(c) = self.chars.peek() {
            if c.is_ascii_alphanumeric() || *c == '_' {
                s.push(*c);
                self.chars.next();
            } else {
                break
            }
        };

        match s.as_str() {
            "var" => Token::Var,
            "int" => Token::Int,
            "while" => Token::While,
            _ => Token::Identifier(s),
        }
    }

    fn read_usize_literal(&mut self) -> Token {
        let mut number = String::new();

        while let Some(c) = self.chars.peek() {
            if !c.is_ascii_digit() {
                break;
            }

            number.push(self.chars.next().unwrap());
        }

        Token::Usize(number.parse().unwrap())
    }

    fn read_comment(&mut self) -> Token {
        let mut comment = String::new();

        while let Some(c) = self.chars.next() {
            if c == '\n' {
                break;
            }

            comment.push(c);
        }

        Token::Comment(comment)
    }

    fn read_operator(&mut self) -> Token {
        let mut operator = String::new();
        operator.push(self.chars.next().unwrap());

        if let Some('=') = self.chars.peek() {
            operator.push(self.chars.next().unwrap());
        }

        match operator.as_str() {
            ">" => Token::GT,
            "<" => Token::LT,
            "=" => Token::Assign,
            "*" => Token::Asterisk,
            "/" => Token::Slash,
            "-" => Token::Minus,
            "+" => Token::Plus,

            ">=" => Token::GEQ,
            "<=" => Token::LEQ,
            "==" => Token::EQ,
            "*=" => Token::TimesEq,
            "/=" => Token::DivideEq,
            "-=" => Token::MinusEq,
            "+=" => Token::PlusEq,

            _ => unreachable!()
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
