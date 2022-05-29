use crate::lexer::source_iterator::SourceIterator;
use crate::lexer::Bracket::{Angle, Curly, Round, Square};
use crate::lexer::Comment::{Docstring, Regular};
use crate::lexer::ErrorKind::{
    BadUnicodeEscape, InvalidCharInEscape, InvalidFloat, TruncatedEscapeSequence, UnknownEscape,
    UnknownStartOfToken, UnterminatedStr,
};
use crate::lexer::Keyword::{
    And, Arr, Bool, False, Float as FloatKeyword, Fn, For, If, Int as IntKeyword, Map, Not, Or,
    Set, Str as StrKeyword, True, While,
};
use crate::lexer::Literal::{Float as FloatLiteral, Int as IntLiteral, Str as StrLiteral};
use crate::lexer::Punctuation::{
    Ampersand, Asterisk, At, Backslash, Caret, CloseBracket, Colon, Comma, Dash, Dollar, Dot,
    Equals, Exclamation, Hashtag, Newline, OpenBracket, Percent, Pipe, Plus, Question, SingleQuote,
    Slash, Tilde,
};
use crate::lexer::{Error, ErrorKind, Result, Token};
use crate::SourceFile;

#[derive(Debug)]
pub struct Tokens<'a> {
    src: SourceIterator<'a>,
}

impl<'a> Tokens<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Self {
            src: SourceIterator::new(source_file),
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<'a, Token>;

    /// Returns Some(Ok(token)) if the next token is valid, Some(Err(e)) if the
    /// next token is invalid, and None if there are no more tokens.
    fn next(&mut self) -> Option<Self::Item> {
        self.src
            .skip_while(|c| c.is_ascii_whitespace() && c != '\n');

        let tok = match self.src.peek()? {
            '/' => Ok(self.read_slash_or_comment()),
            '"' => self.read_str_literal(),

            c if c.is_ascii_digit() => self.read_numeric_literal(),
            c if c.is_alphabetic() || c == '_' => Ok(self.read_identifier_or_kw()),

            // otherwise, consume the peeked char
            _ => match self.src.next().unwrap() {
                '&' => Ok(Token::Punctuation(Ampersand)),
                '*' => Ok(Token::Punctuation(Asterisk)),
                '@' => Ok(Token::Punctuation(At)),
                '\\' => Ok(Token::Punctuation(Backslash)),
                '^' => Ok(Token::Punctuation(Caret)),
                ':' => Ok(Token::Punctuation(Colon)),
                ',' => Ok(Token::Punctuation(Comma)),
                '-' => Ok(Token::Punctuation(Dash)),
                '$' => Ok(Token::Punctuation(Dollar)),
                '.' => Ok(Token::Punctuation(Dot)),
                '=' => Ok(Token::Punctuation(Equals)),
                '!' => Ok(Token::Punctuation(Exclamation)),
                '#' => Ok(Token::Punctuation(Hashtag)),
                '\n' => Ok(Token::Punctuation(Newline)),
                '%' => Ok(Token::Punctuation(Percent)),
                '|' => Ok(Token::Punctuation(Pipe)),
                '+' => Ok(Token::Punctuation(Plus)),
                '?' => Ok(Token::Punctuation(Question)),
                '\'' => Ok(Token::Punctuation(SingleQuote)),
                '~' => Ok(Token::Punctuation(Tilde)),

                '<' => Ok(Token::Punctuation(OpenBracket(Angle))),
                '>' => Ok(Token::Punctuation(CloseBracket(Angle))),
                '{' => Ok(Token::Punctuation(OpenBracket(Curly))),
                '}' => Ok(Token::Punctuation(CloseBracket(Curly))),
                '(' => Ok(Token::Punctuation(OpenBracket(Round))),
                ')' => Ok(Token::Punctuation(CloseBracket(Round))),
                '[' => Ok(Token::Punctuation(OpenBracket(Square))),
                ']' => Ok(Token::Punctuation(CloseBracket(Square))),

                c => Err(self.create_error(UnknownStartOfToken(c))),
            },
        };

        Some(tok)
    }
}

impl<'a> Tokens<'a> {
    /// Determines whether current slash marks start of comment or is just a
    /// Punctuation::Slash, then returns the slash/comment token and advances
    /// self.src.
    ///
    /// Assumption: self.src.next() == Some('/')
    fn read_slash_or_comment(&mut self) -> Token {
        let n_slashes = self.src.count_while(|c| c == '/');
        match n_slashes {
            1 => Token::Punctuation(Slash),
            3 => Token::Comment(Docstring(
                self.src.take_while(|c| c != '\n').trim().to_string(),
            )),
            _ => Token::Comment(Regular(
                self.src.take_while(|c| c != '\n').trim().to_string(),
            )),
        }
    }

    /// Returns next string literal token and advances self.src.
    ///
    /// Assumption: self.src.next() == Some('"').
    fn read_str_literal(&mut self) -> Result<'a, Token> {
        let mut parsing_error = None;
        let mut contents = String::new();

        self.src.skip(1); // skip the first quote

        loop {
            match self.src.peek() {
                Some('\n') | None => return Err(self.create_error(UnterminatedStr)),

                // otherwise, consume the peeked char
                _ => match self.src.next().unwrap() {
                    '"' => {
                        let default = Ok(Token::Literal(StrLiteral(contents)));
                        return parsing_error.unwrap_or(default);
                    }
                    '\\' => match self.parse_escape_in_str_literal() {
                        Ok(c) => contents.push(c),
                        Err(e) => parsing_error = Some(Err(e)),
                    },
                    c => {
                        contents.push(c);
                    }
                },
            }
        }
    }
    /// Parses what follows a single \ in a string literal.
    ///
    /// Assumption: previous self.src.next() == Some('\\')
    fn parse_escape_in_str_literal(&mut self) -> Result<'a, char> {
        match self.src.peek() {
            None | Some('\n') => Err(self.create_error(UnterminatedStr)),

            // otherwise, consume the peeked char
            _ => match self.src.next().unwrap() {
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                't' => Ok('\t'),
                '\\' => Ok('\\'),
                '0' => Ok('\0'),
                '"' => Ok('"'),
                'u' => self.take_hex_code_and_conv_to_char(4),
                'x' => self.take_hex_code_and_conv_to_char(2),
                c => Err(self.create_error(UnknownEscape(c))),
            },
        }
    }

    /// Reads a hex code of code_len hex digits (0-9, a-f) and returns the
    /// character it represents.
    ///
    /// Assumption: self.src.next() == Some(first character of the hex code)
    pub fn take_hex_code_and_conv_to_char(&mut self, code_len: usize) -> Result<'a, char> {
        let mut code = String::new();
        for _ in 0..code_len {
            match self.src.peek() {
                Some('"') => return Err(self.create_error(TruncatedEscapeSequence)),
                None | Some('\n') => return Err(self.create_error(UnterminatedStr)),

                // otherwise, consume the peeked char
                _ => match self.src.next().unwrap() {
                    c if c.is_ascii_hexdigit() => code.push(c),
                    c => return Err(self.create_error(InvalidCharInEscape(c))),
                },
            }
        }
        // Safe to unwrap since from_str_radix() panics if radix > 36 (ours is 16)
        match char::from_u32(u32::from_str_radix(&code, 16).unwrap()) {
            Some(c) => Ok(c),
            None => Err(self.create_error(BadUnicodeEscape(code))),
        }
    }

    /// Returns next numeric literal token (int or float) and advances self.src.
    ///
    /// Assumption: self.src.next() == Some(first digit of a numeric literal)
    fn read_numeric_literal(&mut self) -> Result<'a, Token> {
        let mut num = String::new();

        // Push the first digit (see assumption) to the string
        num.push(self.src.peek().expect("self.src.next() must be a digit"));

        loop {
            let prev = self.src.next();
            let cur = self.src.peek();
            match (prev, cur) {
                (_, Some(c)) if c.is_ascii_digit() => num.push(c),
                (_, Some(c @ ('.' | 'E' | 'e'))) => num.push(c),
                (Some('e' | 'E'), Some(c @ ('-' | '+'))) => num.push(c),
                (_, _) => break,
            }
        }

        if num.contains(['e', 'E', '.']) {
            match num.parse() {
                Ok(parsed) => Ok(Token::Literal(FloatLiteral(parsed))),
                Err(_) => Err(self.create_error(InvalidFloat(num))),
            }
        } else {
            // Safe to unwrap because we can't get an invalid int
            Ok(Token::Literal(IntLiteral(num.parse().unwrap())))
        }
    }

    fn create_error(&self, kind: ErrorKind) -> Error<'a> {
        Error::new(self.src.loc().expect("src.loc() must not be None"), kind)
    }

    /// Returns next identifier or keyword consisting of alphabetic letters, digits,
    /// and underscores.
    ///
    /// , The first letter is not a digit.
    /// Assumption: self.src.next() == Some(first letter of an identifier/keyword)
    fn read_identifier_or_kw(&mut self) -> Token {
        let name = self
            .src
            .take_while(|c| c.is_alphabetic() || c.is_ascii_digit() || c == '_');

        match name.as_str() {
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

            _ => Token::Identifier(name),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::location::Location;
    use crate::TEST_FILE_PATH;

    use super::*;

    macro_rules! assert_source_has_expected_output {
        ($source:expr, $expected:expr) => {{
            let tokens: Vec<_> = Tokens::new($source).collect();
            assert_eq!(tokens, $expected)
        }};
    }

    macro_rules! assert_source_all_ok_and_has_expected_output {
        ($source:expr, $expected:expr) => {{
            let expected: Vec<_> = $expected.into_iter().map(|t| Ok(t)).collect();
            assert_source_has_expected_output!($source, expected)
        }};
    }

    fn create_test_source(source: &str) -> SourceFile {
        SourceFile::new(TEST_FILE_PATH, source)
    }

    fn create_test_error(
        source_file: &SourceFile,
        source_index: usize,
        line: usize,
        row: usize,
        error_kind: ErrorKind,
    ) -> Error {
        Error::new(
            Location::new(source_file, source_index, line, row),
            error_kind,
        )
    }

    #[test]
    fn err_given_unknown_str_escape() {
        let source = create_test_source(r#""\e""#);
        let expected = vec![Err(create_test_error(&source, 2, 1, 3, UnknownEscape('e')))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_truncated_unicode_escape() {
        let source = create_test_source(r#""\u3b9""#);
        let expected = vec![Err(create_test_error(
            &source,
            5,
            1,
            6,
            TruncatedEscapeSequence,
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_unterminated_str_ending_in_unicode_escape() {
        let source = create_test_source(r#""\u3b9"#);
        let expected = vec![Err(create_test_error(&source, 5, 1, 6, UnterminatedStr))];
        assert_source_has_expected_output!(&source, expected);

        let source = create_test_source("\"\\u3b9\n");
        let expected = vec![
            Err(create_test_error(&source, 5, 1, 6, UnterminatedStr)),
            Ok(Token::Punctuation(Newline)),
        ];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_unterminated_str_ending_in_hex_escape() {
        let source = create_test_source(r#""\xa"#);
        let expected = vec![Err(create_test_error(&source, 3, 1, 4, UnterminatedStr))];
        assert_source_has_expected_output!(&source, expected);

        let source = create_test_source("\"\\xa\n");
        let expected = vec![
            Err(create_test_error(&source, 3, 1, 4, UnterminatedStr)),
            Ok(Token::Punctuation(Newline)),
        ];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_invalid_unicode_escape() {
        let source = create_test_source(r#""\u3b9wadsfdsfs""#);
        let expected = vec![Err(create_test_error(
            &source,
            6,
            1,
            7,
            InvalidCharInEscape('w'),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_truncated_hex_escape() {
        let source = create_test_source(r#""\x9""#);
        let expected = vec![Err(create_test_error(
            &source,
            3,
            1,
            4,
            TruncatedEscapeSequence,
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_invalid_hex_escape() {
        let source = create_test_source(r#""\x3z9wadsfdsfs""#);
        let expected = vec![Err(create_test_error(
            &source,
            4,
            1,
            5,
            InvalidCharInEscape('z'),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_unterminated_str_literal() {
        let source = create_test_source("\"test\n\"");

        let expected = vec![
            Err(create_test_error(&source, 4, 1, 5, UnterminatedStr)),
            Ok(Token::Punctuation(Newline)),
            Err(create_test_error(&source, 6, 2, 1, UnterminatedStr)),
        ];

        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_unknown_char() {
        let source = create_test_source("∂");
        let expected = vec![Err(create_test_error(
            &source,
            0,
            1,
            1,
            UnknownStartOfToken('∂'),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_float_with_multiple_e() {
        let source = create_test_source("1.2312E-33333E+9999");
        let expected = vec![Err(create_test_error(
            &source,
            18,
            1,
            19,
            InvalidFloat("1.2312E-33333E+9999".to_string()),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_float_with_consecutive_e() {
        let source = create_test_source("1.2312Ee9999");
        let expected = vec![Err(create_test_error(
            &source,
            11,
            1,
            12,
            InvalidFloat("1.2312Ee9999".to_string()),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_float_with_e_pm() {
        let source = create_test_source("1.2312E+-9999");

        let expected = vec![
            Err(create_test_error(
                &source,
                7,
                1,
                8,
                InvalidFloat("1.2312E+".to_string()),
            )),
            Ok(Token::Punctuation(Dash)),
            Ok(Token::Literal(IntLiteral(9999))),
        ];

        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn err_given_float_with_multiple_decimals() {
        let source = create_test_source("123.456.789");
        let expected = vec![Err(create_test_error(
            &source,
            10,
            1,
            11,
            InvalidFloat("123.456.789".to_string()),
        ))];
        assert_source_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_regular_comment() {
        let source = create_test_source("//      It is way too late    ");
        let expected = vec![Token::Comment(Regular("It is way too late".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_more_than_three_slashes_as_regular_comment() {
        let source = create_test_source("///////////  Thomas is the best!    ");
        let expected = vec![Token::Comment(Regular("Thomas is the best!".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_docstring_comment() {
        let source = create_test_source("/// Max is the best! ");
        let expected = vec![Token::Comment(Docstring("Max is the best!".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_multiline_comment() {
        let source = create_test_source("/// Line 1\n// Line 2");

        let expected = vec![
            Token::Comment(Docstring("Line 1".to_string())),
            Token::Punctuation(Newline),
            Token::Comment(Regular("Line 2".to_string())),
        ];

        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_escape_characters() {
        let source = create_test_source(r#""\\\n\r\t\0\"""#);
        let expected = vec![Token::Literal(StrLiteral("\\\n\r\t\0\"".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_hex_escape_characters() {
        // print(''.join(["\\" + str(hex(ord(c)))[1:] for c in 'flick']))
        let source = create_test_source(r#""\x66\x6c\x69\x63\x6b""#);
        let expected = vec![Token::Literal(StrLiteral("flick".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_unicode_escape_characters() {
        let source = create_test_source(r#""\u2702\u0046\u002f""#);
        let expected = vec![Token::Literal(StrLiteral("✂F/".to_string()))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_short_program() {
        let source = create_test_source("call(3)\nprint(5)");

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

        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_map_statement() {
        let source = create_test_source("map<str, int> m = {\"hi\": 2, \"bye\": 100}");

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

        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_keyword() {
        let source = create_test_source("if");
        let expected = vec![Token::Keyword(If)];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_float_in_scientific_notation_with_big_e() {
        let source = create_test_source("1.1E12");
        let expected = vec![Token::Literal(FloatLiteral(1.1E12))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_float_in_scientific_notation_with_small_e() {
        let source = create_test_source("3.9993e12");
        let expected = vec![Token::Literal(FloatLiteral(3.9993e12))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_float_in_scientific_notation_with_positive_e() {
        let source = create_test_source("123456789E+11");
        let expected = vec![Token::Literal(FloatLiteral(123456789E+11))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_float_in_scientific_notation_with_negative_e() {
        let source = create_test_source("6.67430e-11");
        let expected = vec![Token::Literal(FloatLiteral(6.67430e-11))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_float() {
        let source = create_test_source("123.456789");
        let expected = vec![Token::Literal(FloatLiteral(123.456789))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_negative_float() {
        let source = create_test_source("-123.456789");
        let expected = vec![
            Token::Punctuation(Dash),
            Token::Literal(FloatLiteral(123.456789)),
        ];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_identifier_with_non_ascii_chars() {
        let source = create_test_source("çaí");
        let expected = vec![Token::Identifier("çaí".to_string())];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_number() {
        let source = create_test_source("1234567890");
        let expected = vec![Token::Literal(IntLiteral(1234567890))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_negative_number() {
        let source = create_test_source("-414300");
        let expected = vec![Token::Punctuation(Dash), Token::Literal(IntLiteral(414300))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_identifier() {
        let source = create_test_source("ThomasIsTheBest");
        let expected = vec![Token::Identifier("ThomasIsTheBest".to_string())];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }

    #[test]
    fn parses_str() {
        let source = create_test_source(r#""Max is even better :D""#);
        let expected = vec![Token::Literal(StrLiteral(
            "Max is even better :D".to_string(),
        ))];
        assert_source_all_ok_and_has_expected_output!(&source, expected);
    }
}
