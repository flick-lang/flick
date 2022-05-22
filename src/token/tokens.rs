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
        let i = self.unparsed.find(|c: char| !c.is_whitespace())?;
        self.unparsed = &self.unparsed[i..];

        let c = self.unparsed.chars().next()?;

        let (token, source_len) = match c {
            c if c.is_numeric() => self.take_numeric_literal(),
            c if c.is_alphabetic() || c == '_' => self.take_identifier(),
            '&' => (Token::Punctuation(Ampersand), 1),
            '*' => (Token::Punctuation(Asterisk), 1),
            '@' => (Token::Punctuation(At), 1),
            '^' => (Token::Punctuation(Caret), 1),
            ':' => (Token::Punctuation(Colon), 1),
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
            '\'' =>(Token::Punctuation(SingleQuote), 1),
            '~' => (Token::Punctuation(Tilde), 1),

            '<' => (Token::Punctuation(OpenBracket(Angle)), 1),
            '>' => (Token::Punctuation(OpenBracket(Angle)), 1),
            '{' => (Token::Punctuation(OpenBracket(Curly)), 1),
            '}' => (Token::Punctuation(CloseBracket(Curly)), 1),
            '(' => (Token::Punctuation(OpenBracket(Round)), 1),
            ')' => (Token::Punctuation(CloseBracket(Round)), 1),
            '[' => (Token::Punctuation(OpenBracket(Square)), 1),
            ']' => (Token::Punctuation(CloseBracket(Square)), 1),

            _ => panic!(),
        };

        self.unparsed = &self.unparsed[source_len..];
        Some(token)
    }
}

impl<'a> Tokens<'a> {
    fn take_numeric_literal(&mut self) -> (Token<'a>, usize) {
        use super::Literal::*;

        let non_numeric_index = self.unparsed
            .find(|c: char| !c.is_numeric())
            .unwrap_or(self.unparsed.len());


        if let Some(non_numeric) = self.unparsed.chars().nth(non_numeric_index) {
            match non_numeric {
                'e' | 'E' => todo!(),
                '.' => todo!(),
                _ => {}
            }
        }

        let num = self.unparsed[..non_numeric_index].parse().unwrap();
        (Token::Literal(Int(num)), non_numeric_index)
    }


    fn take_identifier(&mut self) -> (Token<'a>, usize) {
        let first_non_alphanum = self.unparsed
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(self.unparsed.len());

        let name = &self.unparsed[..first_non_alphanum];
        (Token::Identifier(name), first_non_alphanum)
    }
}

#[cfg(test)]
mod tests {
    use super::super::Literal::*;
    use super::*;
    use proptest::prelude::*;

    proptest! {
        // #[test]
        // fn doesnt_crash_with_random_input(s in "\\PC*") {
        //     let tokens: Vec<Token> = Tokens { unparsed: &s }.collect();
        //     // let expected_tokens = vec![]
        // }

        // #[test]
        // fn parses_digit(n in "[0-9]") {
        //     let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
        //     let expected_tokens = vec![Literal(Int(n.parse().unwrap()))];
        //     prop_assert_eq!(tokens, expected_tokens)
        // }

        #[test]
        fn parses_number(n in any::<usize>().prop_map(|n| n.to_string())) {
            let tokens: Vec<_> = Tokens { unparsed: &n }.collect();
            let expected_tokens = vec![Token::Literal(Int(n.parse().unwrap()))];
            prop_assert_eq!(tokens, expected_tokens)
        }
    }
}