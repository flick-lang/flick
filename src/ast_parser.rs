use crate::token::Token;
use crate::token::VarType;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration {
        var_name: String,
        var_type: VarType,
        var_value: Expr,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Statement>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Int(isize),
    BinExpr {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,
}

pub struct ASTParser<'a> {
    tokens: &'a [Token],
    cursor: usize,
}

impl<'a> ASTParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: 0
        }
    }

    fn next_token(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    /// Returns a reference to the next() value without advancing the cursor.
    fn peek_token(&mut self, n: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + (n-1)) // n-1 to fix indexing
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.peek_token(1).is_some() {
            statements.push(self.parse_statement());
        }
        statements
    }

    fn parse_statement(&mut self) -> Statement {
        match self.peek_token(1) {
            Some(Token::Var) => self.parse_var_dec(),
            Some(Token::While) => self.parse_while_loop(),
            _ => unreachable!(),
        }
    }

    // TODO: Maybe make this a macro?
    fn assert_next_token(&mut self, expected: Token) {
        match self.next_token() {
            Some(token) if *token == expected => (),
            Some(token) => unreachable!("unexpected token {:?}, expected {:?}", token, expected),
            None => unreachable!("unexpected end of file, expected {:?}", expected)
        }
    }

    fn parse_var_dec(&mut self) -> Statement {
        self.assert_next_token(Token::Var);

        let var_name = match self.next_token() {
            Some(Token::Identifier(s)) => s.clone(),
            Some(token) => unreachable!("unexpected token {:?}, expected identifier", token),
            None => unreachable!("unexpected end of file, expected identifier")
        };

        self.assert_next_token(Token::Colon);

        let var_type = match self.next_token() {
            Some(Token::VarType(s)) => *s,
            Some(token) => unreachable!("unexpected token {:?}, expected identifier", token),
            None => unreachable!("unexpected end of file, expected identifier")
        };

        self.assert_next_token(Token::Assign);

        let var_value = self.parse_expr();

        Statement::VarDeclaration {
            var_name,
            var_type,
            var_value
        }
    }

    fn parse_while_loop(&mut self) -> Statement {
        self.assert_next_token(Token::While);

        let condition = self.parse_expr();
        let mut body = Vec::new();

        self.assert_next_token(Token::LSquirly);

        while let Some(token) = self.peek_token(1) {
            if *token == Token::RSquirly {
                break
            }
            body.push(self.parse_statement())
        }

        self.assert_next_token(Token::RSquirly);

        Statement::WhileLoop {
            condition,
            body,
        }
    }

    fn parse_expr(&mut self) -> Expr {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast_parser::{ASTParser, BinaryOperator, Expr, Statement};
    use crate::lexer::Lexer;
    use crate::token::{Token, VarType};

    #[test]
    fn var_declaration() {
        let source_code = "var N: int = 5";
        let expected = vec![
            Statement::VarDeclaration {
                var_name: "N".to_string(),
                var_type: VarType::Int,
                var_value: Expr::Int(5),
            }
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn while_loop() {
        let source_code = "while i <= N {\n\t\n}";
        let expected = vec![
            Statement::WhileLoop {
                condition: Expr::BinExpr {
                    left: Box::new(Expr::Identifier("i".to_string())),
                    op: BinaryOperator::LessOrEqualTo,
                    right: Box::new(Expr::Identifier("N".to_string())),
                },
                body: vec![],
            }
        ];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }
}
