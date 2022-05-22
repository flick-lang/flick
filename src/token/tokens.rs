use super::Token;


#[derive(Debug)]
pub struct Tokens<'a> {
    pub(crate) unparsed: &'a str,
}


impl<'a> Iterator for Tokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use super::Punctuation::*;
        use super::Bracket::*;
        // skip whitespace
        let i = self.unparsed.find(|c: char| !c.is_ascii_whitespace())?;
        self.unparsed = &self.unparsed[i..];

        let c = self.unparsed.chars().next()?;

        let (token, source_len) = match c {
            '&' => (Token::Punctuation(Ampersand), 1),
            '*' => (Token::Punctuation(Asterisk), 1),
            '@' => (Token::Punctuation(At), 1),
            '\\' => (Token::Punctuation(Backslash), 1),
            '^' => (Token::Punctuation(Caret), 1),
            ':' => (Token::Punctuation(Colon), 1),
            ',' => (Token::Punctuation(Comma), 1),
            '-' => (Token::Punctuation(Dash), 1),
            '$' => (Token::Punctuation(Dollar), 1),
            '.' => (Token::Punctuation(Dot), 1),
            '"' => (Token::Punctuation(DoubleQuote), 1),
            '=' => (Token::Punctuation(Equal), 1),
            '!' => (Token::Punctuation(Exclamation), 1),
            '#' => (Token::Punctuation(Hashtag), 1),
            '%' => (Token::Punctuation(Percent), 1),
            '|' => (Token::Punctuation(Pipe), 1),
            '+' => (Token::Punctuation(Plus), 1),
            '?' => (Token::Punctuation(Question), 1),
            '\'' => (Token::Punctuation(SingleQuote), 1),
            '/' => (Token::Punctuation(Slash), 1),
            '~' => (Token::Punctuation(Tilde), 1),

            '<' => (Token::Punctuation(OpenBracket(Angle)), 1),
            '>' => (Token::Punctuation(OpenBracket(Angle)), 1),
            '{' => (Token::Punctuation(OpenBracket(Curly)), 1),
            '}' => (Token::Punctuation(CloseBracket(Curly)), 1),
            '(' => (Token::Punctuation(OpenBracket(Round)), 1),
            ')' => (Token::Punctuation(CloseBracket(Round)), 1),
            '[' => (Token::Punctuation(OpenBracket(Square)), 1),
            ']' => (Token::Punctuation(CloseBracket(Square)), 1),

            c if c.is_ascii_digit() => self.read_numeric_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.read_identifier_or_kw(),

            c => (Token::Unknown(c), c.len_utf8()),
        };

        self.unparsed = &self.unparsed[source_len..];
        Some(token)
    }
}

impl<'a> Tokens<'a> {
    fn read_numeric_literal(&mut self) -> (Token<'a>, usize) {
        use super::Literal::Int;

        let source_len = self.unparsed
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(self.unparsed.len());


        if let Some(non_numeric) = self.unparsed.chars().nth(source_len) {
            match non_numeric {
                'e' | 'E' => todo!(),
                '.' => todo!(),
                _ => {}
            }
        }

        let num = self.unparsed[..source_len].parse().unwrap();
        (Token::Literal(Int(num)), source_len)
    }


    fn read_identifier_or_kw(&mut self) -> (Token<'a>, usize) {
        use super::Keyword::*;

        let source_len = self.unparsed
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
            .unwrap_or(self.unparsed.len());

        let name = &self.unparsed[..source_len];
        let token = match name {
            "arr" => Token::Keyword(Arr),
            "bool" => Token::Keyword(Bool),
            "float" => Token::Keyword(Float),
            "int" => Token::Keyword(Int),
            "map" => Token::Keyword(Map),
            "set" => Token::Keyword(Set),
            "str" => Token::Keyword(Str),

            "and" => Token::Keyword(And),
            "false" => Token::Keyword(False),
            "not" => Token::Keyword(Not),
            "or" => Token::Keyword(Or),
            "true" => Token::Keyword(True),

            "fn" => Token::Keyword(Fn),
            "for" => Token::Keyword(For),
            "if" => Token::Keyword(If),
            "while" => Token::Keyword(While),

            _ => Token::Identifier(name)
        };
        (token, source_len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn parses_short_program() {
        use super::super::Punctuation::{OpenBracket, CloseBracket};
        use super::super::Bracket::Round;
        use super::super::Literal::Int;
        let source = "call(3)\nprint(5)";
        let tokens: Vec<_> = Tokens { unparsed: source }.collect();
        let expected = vec![
            Token::Identifier("call"),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(Int(3)),
            Token::Punctuation(CloseBracket(Round)),
            Token::Identifier("print"),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(Int(5)),
            Token::Punctuation(CloseBracket(Round)),
        ];
        assert_eq!(tokens, expected)
    }

    #[test]
    fn parses_keyword() {
        use super::super::Keyword::If;
        let tokens: Vec<_> = Tokens { unparsed: "if" }.collect();
        let expected = vec![Token::Keyword(If)];
        assert_eq!(tokens, expected);
    }

    proptest! {
        #[test]
        fn doesnt_crash_parsing_random_char(s in "\\PC") {
            let _: Vec<_> = Tokens { unparsed: &s }.collect();
        }

        // #[test]
        // fn doesnt_crash_parsing_random_char(s in "\\PC*") {
        //     let _: Vec<_> = Tokens { unparsed: &s }.collect();
        // }

        #[test]
        fn parses_number(n in any::<usize>().prop_map(|n| n.to_string())) {
            use super::super::Literal::Int;
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Literal(Int(n.parse().unwrap()))];
            prop_assert_eq!(tokens, expected)
        }

        #[test]
        fn parses_identifier(n in "[a-zA-Z_][a-zA-Z0-9_]*") {
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Identifier(&n)];
            prop_assert_eq!(tokens, expected)
        }
    }
}