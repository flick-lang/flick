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

        let mut iter = self.unparsed.chars().peekable();

        let (token, source_len) = match (iter.next()?, iter.peek()) {
            ('/', Some('/')) => self.read_comment(),
            ('"', _) => self.read_string_literal(),

            ('&', _) => (Token::Punctuation(Ampersand), 1),
            ('*', _) => (Token::Punctuation(Asterisk), 1),
            ('@', _) => (Token::Punctuation(At), 1),
            ('\\', _) => (Token::Punctuation(Backslash), 1),
            ('^', _) => (Token::Punctuation(Caret), 1),
            (':', _) => (Token::Punctuation(Colon), 1),
            (',', _) => (Token::Punctuation(Comma), 1),
            ('-', _) => (Token::Punctuation(Dash), 1),
            ('$', _) => (Token::Punctuation(Dollar), 1),
            ('.', _) => (Token::Punctuation(Dot), 1),
            ('=', _) => (Token::Punctuation(Equals), 1),
            ('!', _) => (Token::Punctuation(Exclamation), 1),
            ('#', _) => (Token::Punctuation(Hashtag), 1),
            ('\n', _) => (Token::Punctuation(Newline), 1),
            ('%', _) => (Token::Punctuation(Percent), 1),
            ('|', _) => (Token::Punctuation(Pipe), 1),
            ('+', _) => (Token::Punctuation(Plus), 1),
            ('?', _) => (Token::Punctuation(Question), 1),
            ('\'', _) => (Token::Punctuation(SingleQuote), 1),
            ('/', _) => (Token::Punctuation(Slash), 1),
            ('~', _) => (Token::Punctuation(Tilde), 1),

            ('<', _) => (Token::Punctuation(OpenBracket(Angle)), 1),
            ('>', _) => (Token::Punctuation(CloseBracket(Angle)), 1),
            ('{', _) => (Token::Punctuation(OpenBracket(Curly)), 1),
            ('}', _) => (Token::Punctuation(CloseBracket(Curly)), 1),
            ('(', _) => (Token::Punctuation(OpenBracket(Round)), 1),
            (')', _) => (Token::Punctuation(CloseBracket(Round)), 1),
            ('[', _) => (Token::Punctuation(OpenBracket(Square)), 1),
            (']', _) => (Token::Punctuation(CloseBracket(Square)), 1),

            (c, _) if c.is_ascii_digit() => self.read_numeric_literal(),
            (c, _) if c.is_ascii_alphabetic() || c == '_' => self.read_identifier_or_kw(),

            (c, _) => panic!("Unknown character '{}'", c)
        };

        self.unparsed = &self.unparsed[source_len..];
        Some(token)
    }
}

impl<'a> Tokens<'a> {
    fn read_comment(&mut self) -> (Token, usize) {
        use super::Comment::{Docstring, Regular};

        let source_len = self.unparsed.find('\n').unwrap_or(self.unparsed.len());

        let n_slashes = self.unparsed.find(|c| c != '/').unwrap_or(self.unparsed.len());
        let comment_body = self.unparsed[n_slashes..source_len].trim().to_string();

        match n_slashes {
            3 => (Token::Comment(Docstring(comment_body)), source_len),
            _ => (Token::Comment(Regular(comment_body)), source_len),
        }
    }

    fn read_string_literal(&mut self) -> (Token, usize) {
        use super::Literal::Str;

        let mut contents = String::new();

        // We skip the first quote in the iterator
        let mut iter = self.unparsed.chars().enumerate().skip(1);

        let source_len = loop {  // loop is broken when quote is read
            match iter.next() {
                None => Self::eof_before_closing_quote(),
                Some((i, c)) => match c {
                    '"' => break i + 1,
                    '\\' => contents.push(Self::parse_escape_in_str_literal(&mut iter)),
                    c => contents.push(c)
                },
            }
        };

        // Plus two because of the closing quotes on both sides
        (Token::Literal(Str(contents)), source_len)
    }

    fn parse_escape_in_str_literal(iter: &mut impl Iterator<Item=(usize, char)>) -> char {
        match iter.next() {
            Some((_, c)) => {
                match c {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '0' => '\0',
                    '"' => '"',
                    'u' => Self::parse_hex_str(iter.take(4).map(|(_, c)| c).collect()),
                    'x' => Self::parse_hex_str(iter.take(2).map(|(_, c)| c).collect()),
                    c => panic!("Unknown character escape '{}'", c),
                }
            }
            None => Self::eof_before_closing_quote(),
        }
    }

    fn parse_hex_str(hex_str: String) -> char {
        char::from_u32(u32::from_str_radix(&hex_str, 16).unwrap()).unwrap()
    }

    fn eof_before_closing_quote() -> ! {
        panic!("no closing quote before EOF");
    }

    fn read_numeric_literal(&mut self) -> (Token, usize) {
        use super::Literal::{Int, Float};

        // While we never check the very first digit, we know it is going to be a digit because otherwise this function would never have been called
        let source_len = self.unparsed
            .chars()
            .zip(self.unparsed.chars().skip(1))
            .enumerate()
            .find(|&(_, (prev, cur))| !(cur.is_ascii_digit() || cur == '.' || cur == 'E' || cur == 'e' || ((cur == '-' || cur == '+') && (prev == 'E' || prev == 'e'))))
            .map(|(i, _)| i+1)
            .unwrap_or(self.unparsed.len());

        let num = &self.unparsed[..source_len];

        if num.contains(|c| c == 'E' || c == 'e' || c == '.') {
            (Token::Literal(Float(num.parse().unwrap())), source_len)
        } else {
            (Token::Literal(Int(num.parse().unwrap())), source_len)
        }
    }

    fn read_identifier_or_kw(&mut self) -> (Token, usize) {
        use super::Keyword::*;

        let source_len = self.unparsed
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
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
    fn parses_regular_comment() {
        use super::super::Comment::Regular;
        let tokens: Vec<_> = Tokens { unparsed: "//       It is way too late for this       " }.collect();
        let expected = vec![Token::Comment(Regular("It is way too late for this".to_string()))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_more_than_three_slashes_as_regular_comment() {
        use super::super::Comment::Regular;
        let tokens: Vec<_> = Tokens { unparsed: "/////////////////////               Thomas is the best coder in the world!         " }.collect();
        let expected = vec![Token::Comment(Regular("Thomas is the best coder in the world!".to_string()))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_docstring_comment() {
        use super::super::Comment::Docstring;
        let tokens: Vec<_> = Tokens { unparsed: "/// Max is the best coder in the world!" }.collect();
        let expected = vec![Token::Comment(Docstring("Max is the best coder in the world!".to_string()))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_multiline_comment() {
        use super::super::Comment::{Docstring, Regular};
        use super::super::Punctuation::Newline;
        let tokens: Vec<_> = Tokens { unparsed: "/// Line 1\n// Line 2" }.collect();
        let expected = vec![
            Token::Comment(Docstring("Line 1".to_string())),
            Token::Punctuation(Newline),
            Token::Comment(Regular("Line 2".to_string()))
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_escape_characters() {
        use super::super::Literal::Str;
        let tokens: Vec<_> = Tokens { unparsed: r#""\\\n\r\t\0\"""# }.collect();
        let expected = vec![Token::Literal(Str("\\\n\r\t\0\"".to_string()))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_hex_escape_characters() {
        use super::super::Literal::Str;
        // print(''.join(["\\" + str(hex(ord(c)))[1:] for c in 'flick']))
        let tokens: Vec<_> = Tokens { unparsed: r#""\x66\x6c\x69\x63\x6b""# }.collect();
        let expected = vec![Token::Literal(Str("flick".to_string()))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_unicode_escape_characters() {
        use super::super::Literal::Str;
        let tokens: Vec<_> = Tokens { unparsed: r#""\u2702\u0046\u002f""# }.collect();
        let expected = vec![Token::Literal(Str("\u{2702}\u{0046}\u{002f}".to_string()))];
        assert_eq!(tokens, expected);
    }

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

    #[test]
    fn parses_floats_in_scientific_notation_with_big_e() {
        use super::super::Literal::Float;
        let tokens: Vec<_> = Tokens { unparsed: "1.1E12" }.collect();
        let expected = vec![Token::Literal(Float(1.1E12))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_small_e() {
        use super::super::Literal::Float;
        let tokens: Vec<_> = Tokens { unparsed: "3.9993e12" }.collect();
        let expected = vec![Token::Literal(Float(3.9993e12))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_positive_e() {
        use super::super::Literal::Float;
        let tokens: Vec<_> = Tokens { unparsed: "123456789E+11" }.collect();
        let expected = vec![Token::Literal(Float(123456789E+11))];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_negative_e() {
        use super::super::Literal::Float;
        let tokens: Vec<_> = Tokens { unparsed: "6.67430e-11" }.collect();
        let expected = vec![Token::Literal(Float(6.67430e-11))];
        assert_eq!(tokens, expected);
    }

    proptest! {
        #[test]
        fn parses_numbers(n in any::<usize>().prop_map(|n| n.to_string())) {
            use super::super::Literal::Int;
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Literal(Int(n.parse().unwrap()))];
            prop_assert_eq!(tokens, expected)
        }

        #[test]
        fn parses_float(mut n in proptest::num::f64::POSITIVE.prop_map(|n| n.to_string())) {
            use super::super::Literal::Float;

            if !n.contains(|c| c == '.' || c == 'E' || c == 'e') {
                n.push('.'); // To avoid getting parsed as int
            }

            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Literal(Float(n.parse().unwrap()))];
            prop_assert_eq!(tokens, expected)
        }

        #[test]
        fn parses_identifiers(n in "[a-zA-Z_][a-zA-Z0-9_]*") {
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Identifier(n)];
            prop_assert_eq!(tokens, expected)
        }

        #[test]
        fn parses_strings_without_escapes(n in r#""[a-zA-Z0-9_]*""#) {
            use super::super::Literal::Str;
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected = vec![Token::Literal(Str(n[1..n.len()-1].to_string()))];
            prop_assert_eq!(tokens, expected)
        }
    }
}