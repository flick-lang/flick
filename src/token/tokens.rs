use crate::token::Bracket::{Angle, Curly, Round, Square};
use crate::token::Comment::{Docstring, Regular};
use crate::token::Keyword::{
    And, Arr, Bool, False, Float as FloatKeyword, Fn, For, If, Int as IntKeyword, Map, Not, Or,
    Set, Str as StrKeyword, True, While,
};
use crate::token::Literal::{Float as FloatLiteral, Int as IntLiteral, Str as StrLiteral};
use crate::token::Punctuation::{
    Ampersand, Asterisk, At, Backslash, Caret, CloseBracket, Colon, Comma, Dash, Dollar, Dot,
    Equals, Exclamation, Hashtag, Newline, OpenBracket, Percent, Pipe, Plus, Question, SingleQuote,
    Slash, Tilde,
};
use crate::token::Token;

#[derive(Debug)]
pub struct Tokens<'a> {
    pub(crate) unparsed: &'a str,
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespace
        let i = self
            .unparsed
            .find(|c: char| !c.is_ascii_whitespace() || c == '\n')?;
        self.unparsed = &self.unparsed[i..];

        let mut iter = self.unparsed.chars().peekable();

        let cur = iter.next()?;
        let next = iter.peek();
        let (token, source_len) = match (cur, next) {
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

            (c, _) => panic!("Unknown character '{}'", c),
        };

        self.unparsed = &self.unparsed[source_len..];
        Some(token)
    }
}

impl<'a> Tokens<'a> {
    fn read_comment(&mut self) -> (Token, usize) {
        let n_slashes = self
            .unparsed
            .find(|c| c != '/')
            .unwrap_or(self.unparsed.len());

        let source_len = self.unparsed.find('\n').unwrap_or(self.unparsed.len());

        let comment_body = self.unparsed[n_slashes..source_len].trim().to_string();

        match n_slashes {
            3 => (Token::Comment(Docstring(comment_body)), source_len),
            _ => (Token::Comment(Regular(comment_body)), source_len),
        }
    }

    fn read_string_literal(&mut self) -> (Token, usize) {
        let mut contents = String::new();

        // We skip the first quote
        let mut iter = self.unparsed.chars().enumerate().skip(1).peekable();

        let source_len = loop {
            match iter.next() {
                Some((i, c)) => match c {
                    '"' => break i + 1,
                    '\\' => contents.push(Self::parse_escape_in_str_literal(&mut iter)),
                    c => contents.push(c),
                },
                _ => Self::eof_before_closing_quote(),
            }
        };

        (Token::Literal(StrLiteral(contents)), source_len)
    }

    fn parse_escape_in_str_literal(iter: &mut impl Iterator<Item = (usize, char)>) -> char {
        match iter.next() {
            Some((_, c)) => match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '0' => '\0',
                '"' => '"',
                'u' => Self::parse_hex_str(iter.take(4).map(|(_, c)| c).collect()),
                'x' => Self::parse_hex_str(iter.take(2).map(|(_, c)| c).collect()),
                c => panic!("Unknown character escape '{}'", c),
            },
            None => Self::eof_before_closing_quote(),
        }
    }

    fn eof_before_closing_quote() -> ! {
        panic!("no closing quote before EOF")
    }

    fn parse_hex_str(hex_str: String) -> char {
        char::from_u32(u32::from_str_radix(&hex_str, 16).unwrap()).unwrap()
    }

    fn read_numeric_literal(&mut self) -> (Token, usize) {
        // While we never check the very first digit, we know it is going to be a digit because otherwise this function would never have been called
        let source_len = self
            .unparsed
            .chars()
            .zip(self.unparsed.chars().skip(1))
            .enumerate()
            .find(|&(_, (prev, cur))| {
                !(cur.is_ascii_digit()
                    || cur == '.'
                    || cur == 'E'
                    || cur == 'e'
                    || ((cur == '-' || cur == '+') && (prev == 'E' || prev == 'e')))
            })
            .map(|(i, _)| i + 1)
            .unwrap_or(self.unparsed.len());

        let num = &self.unparsed[..source_len];

        if num.contains(|c| c == 'E' || c == 'e' || c == '.') {
            (
                Token::Literal(FloatLiteral(num.parse().unwrap())),
                source_len,
            )
        } else {
            (Token::Literal(IntLiteral(num.parse().unwrap())), source_len)
        }
    }

    fn read_identifier_or_kw(&mut self) -> (Token, usize) {
        let source_len = self
            .unparsed
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .unwrap_or(self.unparsed.len());

        let name = &self.unparsed[..source_len];

        let token = match name {
            "arr" => Token::Keyword(Arr),
            "bool" => Token::Keyword(Bool),
            "float" => Token::Keyword(FloatKeyword),
            "int" => Token::Keyword(IntKeyword),
            "map" => Token::Keyword(Map),
            "set" => Token::Keyword(Set),
            "str" => Token::Keyword(StrKeyword),

            "and" => Token::Keyword(And),
            "false" => Token::Keyword(False),
            "not" => Token::Keyword(Not),
            "or" => Token::Keyword(Or),
            "true" => Token::Keyword(True),

            "fn" => Token::Keyword(Fn),
            "for" => Token::Keyword(For),
            "if" => Token::Keyword(If),
            "while" => Token::Keyword(While),

            _ => Token::Identifier(name.to_string()),
        };
        (token, source_len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    macro_rules! assert_source_has_expected_output {
        ($source:expr, $expected:expr) => {{
            let tokens: Vec<_> = Tokens { unparsed: $source }.collect();
            assert_eq!(tokens, $expected)
        }};
    }

    macro_rules! prop_assert_source_has_expected_output {
        ($source:expr, $expected:expr) => {{
            let tokens: Vec<_> = Tokens { unparsed: $source }.collect();
            prop_assert_eq!(tokens, $expected)
        }};
    }

    #[test]
    fn parses_regular_comment() {
        let source = "//      It is way too late    ";
        let expected = vec![Token::Comment(Regular("It is way too late".to_string()))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_more_than_three_slashes_as_regular_comment() {
        let source = "///////////  Thomas is the best!    ";
        let expected = vec![Token::Comment(Regular("Thomas is the best!".to_string()))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_docstring_comment() {
        let source = "/// Max is the best! ";
        let expected = vec![Token::Comment(Docstring("Max is the best!".to_string()))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_multiline_comment() {
        let source = "/// Line 1\n// Line 2";

        let expected = vec![
            Token::Comment(Docstring("Line 1".to_string())),
            Token::Punctuation(Newline),
            Token::Comment(Regular("Line 2".to_string())),
        ];

        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_escape_characters() {
        let source = r#""\\\n\r\t\0\"""#;
        let expected = vec![Token::Literal(StrLiteral("\\\n\r\t\0\"".to_string()))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_hex_escape_characters() {
        // print(''.join(["\\" + str(hex(ord(c)))[1:] for c in 'flick']))
        let source = r#""\x66\x6c\x69\x63\x6b""#;
        let expected = vec![Token::Literal(StrLiteral("flick".to_string()))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_unicode_escape_characters() {
        let source = r#""\u2702\u0046\u002f""#;

        let expected = vec![Token::Literal(StrLiteral(
            "\u{2702}\u{0046}\u{002f}".to_string(),
        ))];

        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_short_program() {
        let source = "call(3)\nprint(5)";
        let expected = vec![
            Token::Identifier("call".to_string()),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(IntLiteral(3)),
            Token::Punctuation(CloseBracket(Round)),
            Token::Punctuation(Newline),
            Token::Identifier("print".to_string()),
            Token::Punctuation(OpenBracket(Round)),
            Token::Literal(IntLiteral(5)),
            Token::Punctuation(CloseBracket(Round)),
        ];

        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_map_statement() {
        let source = "map<str, int> m = {\"hi\": 2, \"bye\": 100}";

        let expected = vec![
            Token::Keyword(Map),
            Token::Punctuation(OpenBracket(Angle)),
            Token::Keyword(StrKeyword),
            Token::Punctuation(Comma),
            Token::Keyword(IntKeyword),
            Token::Punctuation(CloseBracket(Angle)),
            Token::Identifier("m".to_string()),
            Token::Punctuation(Equals),
            Token::Punctuation(OpenBracket(Curly)),
            Token::Literal(StrLiteral("hi".to_string())),
            Token::Punctuation(Colon),
            Token::Literal(IntLiteral(2)),
            Token::Punctuation(Comma),
            Token::Literal(StrLiteral("bye".to_string())),
            Token::Punctuation(Colon),
            Token::Literal(IntLiteral(100)),
            Token::Punctuation(CloseBracket(Curly)),
        ];

        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_keyword() {
        let source = "if";
        let expected = vec![Token::Keyword(If)];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_big_e() {
        let source = "1.1E12";
        let expected = vec![Token::Literal(FloatLiteral(1.1E12))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_small_e() {
        let source = "3.9993e12";
        let expected = vec![Token::Literal(FloatLiteral(3.9993e12))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_positive_e() {
        let source = "123456789E+11";
        let expected = vec![Token::Literal(FloatLiteral(123456789E+11))];
        assert_source_has_expected_output!(source, expected)
    }

    #[test]
    fn parses_floats_in_scientific_notation_with_negative_e() {
        let source = "6.67430e-11";
        let expected = vec![Token::Literal(FloatLiteral(6.67430e-11))];
        assert_source_has_expected_output!(source, expected)
    }

    proptest! {
        #[test]
        fn parses_numbers(source in any::<usize>().prop_map(|n| n.to_string())) {
            let expected = vec![Token::Literal(IntLiteral(source.parse().unwrap()))];
            prop_assert_source_has_expected_output!(&source, expected)
        }

        #[test]
        fn parses_float(mut source in proptest::num::f64::POSITIVE.prop_map(|f| f.to_string())) {
            if !source.contains(|c| c == '.' || c == 'E' || c == 'e') {
                source.push('.'); // To avoid getting parsed as int
            }

            let expected = vec![Token::Literal(FloatLiteral(source.parse().unwrap()))];
            prop_assert_source_has_expected_output!(&source, expected)
        }

        #[test]
        fn parses_identifiers(source in "[a-zA-Z_][a-zA-Z0-9_]*") {
            let expected = vec![Token::Identifier(source.clone())];
            prop_assert_source_has_expected_output!(&source, expected)
        }

        #[test]
        fn parses_strings_without_escapes(source in r#""[a-zA-Z0-9_]*""#) {
            let expected = vec![Token::Literal(StrLiteral(source[1..source.len()-1].to_string()))];
            prop_assert_source_has_expected_output!(&source, expected)
        }
    }
}
