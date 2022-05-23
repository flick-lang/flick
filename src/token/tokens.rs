use super::Token;


#[derive(Debug)]
pub struct Tokens<'a> {
    pub(crate) unparsed: &'a str,
}


impl<'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use super::Punctuation::*;
        use super::Bracket::*;
        // skip whitespace
        let i = self.unparsed.find(|c: char| !c.is_ascii_whitespace() || c == '\n')?;
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
            '=' => (Token::Punctuation(Equals), 1),
            '!' => (Token::Punctuation(Exclamation), 1),
            '#' => (Token::Punctuation(Hashtag), 1),
            '\n' => (Token::Punctuation(Newline), 1),
            '%' => (Token::Punctuation(Percent), 1),
            '|' => (Token::Punctuation(Pipe), 1),
            '+' => (Token::Punctuation(Plus), 1),
            '?' => (Token::Punctuation(Question), 1),
            '\'' => (Token::Punctuation(SingleQuote), 1),
            '/' => (Token::Punctuation(Slash), 1),
            '~' => (Token::Punctuation(Tilde), 1),

            '<' => (Token::Punctuation(OpenBracket(Angle)), 1),
            '>' => (Token::Punctuation(CloseBracket(Angle)), 1),
            '{' => (Token::Punctuation(OpenBracket(Curly)), 1),
            '}' => (Token::Punctuation(CloseBracket(Curly)), 1),
            '(' => (Token::Punctuation(OpenBracket(Round)), 1),
            ')' => (Token::Punctuation(CloseBracket(Round)), 1),
            '[' => (Token::Punctuation(OpenBracket(Square)), 1),
            ']' => (Token::Punctuation(CloseBracket(Square)), 1),

            '"' => self.read_string_literal(),
            c if c.is_ascii_digit() => self.read_numeric_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.read_identifier_or_kw(),

            c => (Token::Unknown(c), c.len_utf8()),
        };

        self.unparsed = &self.unparsed[source_len..];
        Some(token)
    }
}

impl<'a> Tokens<'a> {
    fn read_string_literal(&mut self) -> (Token, usize) {
        use super::Literal::Str;

        let mut contents = String::new();

        // We skip the first quote in the iterator
        let mut iter = self.unparsed.chars().skip(1).enumerate();

        let source_len = loop {
            match iter.next() {
                Some((i, c)) => match c {
                    '\\' => match iter.next() {
                        Some((_, c)) => {
                            let c = match c {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                '0' => '\0',
                                '"' => '"',
                                // iter.take(2).collect() does not work because it consumer iter, which means we can't use it for the next iteration
                                // we could implement own trait on iterator, like "take_without_owning" that doesn't create a struct and instead just
                                // calls next 4 times and returns that? or a custom function that creates a new iterator that references original iterator
                                // TODO: ALSO, this is duplicated code so let's NOT duplicate it
                                'u' => {
                                    // If this panics, EOF was reached before all 4 unicode bytes
                                    let mut hex = String::with_capacity(4);
                                    for _ in 0..4 {
                                        hex.push(iter.next().unwrap().1)
                                    }
                                    // This can panic in two places
                                    char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap()
                                },
                                'x' => {
                                    // If this panics, EOF was reached before all 4 unicode bytes
                                    let mut hex = String::with_capacity(4);
                                    for _ in 0..2 {
                                        hex.push(iter.next().unwrap().1)
                                    }
                                    // This can panic in two places
                                    char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap()
                                },
                                _ => panic!("Unknown character escape")
                            };
                            contents.push(c);
                        }
                        None => {}
                    },
                    '"' => break i,
                    c => contents.push(c)
                },
                None => panic!("no closing quote before EOF")
            }
        };

        // Plus two because of the closing quotes on both sides
        (Token::Literal(Str(contents)), source_len+2)
    }

    fn read_numeric_literal(&mut self) -> (Token, usize) {
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

    fn read_identifier_or_kw(&mut self) -> (Token, usize) {
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

            _ => Token::Identifier(name.to_string())
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
        use super::super::Punctuation::{OpenBracket, CloseBracket, Newline};
        use super::super::Bracket::Round;
        use super::super::Literal::Int;
        let source = "call(3)\nprint(5)";
        let tokens: Vec<_> = Tokens { unparsed: source }.collect();
        let expected = vec![
            Token::Identifier("call".to_string()),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(Int(3)),
            Token::Punctuation(CloseBracket(Round)),
            Token::Punctuation(Newline),
            Token::Identifier("print".to_string()),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(Int(5)),
            Token::Punctuation(CloseBracket(Round)),
        ];
        assert_eq!(tokens, expected)
    }


    #[test]
    fn parses_map_statement() {
        use super::super::Keyword::{self, Map};
        use super::super::Bracket::{Angle, Curly};
        use super::super::Literal;
        use super::super::Punctuation::{Equals, OpenBracket, CloseBracket, Comma, Colon};

        let source = "map<str, int> m = {\"hi\": 2, \"bye\": 100}";
        let tokens: Vec<_> = Tokens { unparsed: source }.collect();
        let expected = vec![
            Token::Keyword(Map),
            Token::Punctuation(OpenBracket(Angle)),
            Token::Keyword(Keyword::Str),
            Token::Punctuation(Comma),
            Token::Keyword(Keyword::Int),
            Token::Punctuation(CloseBracket(Angle)),
            Token::Identifier("m".to_string()),
            Token::Punctuation(Equals),
            Token::Punctuation(OpenBracket(Curly)),
            Token::Literal(Literal::Str("hi".to_string())),
            Token::Punctuation(Colon),
            Token::Literal(Literal::Int(2)),
            Token::Punctuation(Comma),
            Token::Literal(Literal::Str("bye".to_string())),
            Token::Punctuation(Colon),
            Token::Literal(Literal::Int(100)),
            Token::Punctuation(CloseBracket(Curly)),
        ];

        assert_eq!(tokens, expected);
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
            prop_assume!(s != "\"");
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
            let expected = vec![Token::Identifier(n)];
            prop_assert_eq!(tokens, expected)
        }

        #[test]
        fn parses_string_without_escapes(n in "\"[a-zA-Z0-9_]*\"") {
            use super::super::Literal::Str;
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Literal(Str(n[1..n.len()-1].to_string()))];
            prop_assert_eq!(tokens, expected)
        }
    }
}