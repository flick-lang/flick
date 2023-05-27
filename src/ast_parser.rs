use crate::token::OperatorSymbol;
use crate::token::OperatorSymbol::*;
use crate::token::Token;
use crate::token::VarType;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    VarDeclaration {
        var_name: Expr,
        var_type: VarType,
        var_value: Expr,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Statement>,
    },
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Int(isize),
    BinExpr {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    CallExpr {
        function_name: Box<Expr>,
        args: Vec<Expr>,
    },
    IndexExpr {
        container: Box<Expr>,
        index: (),
    },
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    NotEqualTo,
    EqualTo,
    LessThan,
    GreaterThan,
    LessOrEqualTo,
    GreaterOrEqualTo,

    PlusEq,
    TimesEq,
    MinusEq,
    DivideEq,
    Assign,
    // LogicalAnd,
    // LogicalOr,
}

impl From<OperatorSymbol> for BinaryOperator {
    fn from(operator: OperatorSymbol) -> Self {
        match operator {
            Plus => Self::Add,
            Minus => Self::Subtract,
            Asterisk => Self::Multiply,
            Slash => Self::Divide,

            NotEqualTo => Self::NotEqualTo,
            EqualTo => Self::EqualTo,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessOrEqualTo => Self::LessOrEqualTo,
            GreaterOrEqualTo => Self::GreaterOrEqualTo,

            PlusEq => Self::PlusEq,
            TimesEq => Self::TimesEq,
            MinusEq => Self::MinusEq,
            DivideEq => Self::DivideEq,
            Assign => Self::Assign,
        }
    }
}

pub struct ASTParser<'a> {
    tokens: &'a [Token],
    cursor: usize,
}

impl<'a> ASTParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn next_token(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    /// Returns a reference to the next() value without advancing the cursor.
    fn peek_token(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + (n - 1)) // n-1 to fix indexing
    }

    fn skip_token(&mut self) {
        self.cursor += 1;
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while let Some(statement) = self.parse_statement() {
            statements.push(statement);
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        // todo take into account the fact that docstring CAN appear in parse tree
        // enjoy this beautiful formatting <3
        while let Some(Token::Newline | Token::Comment(_) | Token::Docstring(_)) =
            self.peek_token(1)
        {
            self.skip_token();
        }

        let statement = match self.peek_token(1)? {
            Token::VarType(_) => self.parse_var_dec(),
            Token::While => self.parse_while_loop(),
            _ => Statement::Expr(self.parse_expr()),
        };

        match self.next_token() {
            Some(Token::Newline) | None => Some(statement),
            Some(token) => panic!("unexpected token {:?}, expected newline or EOF", token),
        }
    }

    // TODO: Maybe make this a macro?
    fn assert_next_token(&mut self, expected: Token) {
        match self.next_token() {
            Some(token) if *token == expected => (),
            Some(token) => panic!("unexpected token {:?}, expected {:?}", token, expected),
            None => panic!("unexpected end of file, expected {:?}", expected),
        }
    }

    fn parse_var_dec(&mut self) -> Statement {
        let var_type = match self.next_token() {
            Some(Token::VarType(var_type)) => *var_type,
            Some(t) => unreachable!("parse_var_dec but token is {:?} (expected VarType)", t),
            None => unreachable!("parse_var_dec but file ended (expected VarType)"),
        };

        let var_name = self.parse_primary_expr();

        self.assert_next_token(Token::OperatorSymbol(Assign));

        let var_value = self.parse_expr();

        Statement::VarDeclaration {
            var_name,
            var_type,
            var_value,
        }
    }

    fn parse_while_loop(&mut self) -> Statement {
        self.assert_next_token(Token::While);

        let condition = self.parse_expr();
        let mut body = Vec::new();

        self.assert_next_token(Token::LSquirly);

        while let Some(token) = self.peek_token(1) {
            if *token == Token::RSquirly {
                break;
            }

            match self.parse_statement() {
                Some(statement) => body.push(statement),
                None => {
                    panic!("Unexpected end of file, while loop should have a closing squirly")
                }
            }
        }

        self.assert_next_token(Token::RSquirly);

        Statement::WhileLoop { condition, body }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> Expr {
        let left = self.parse_logical_or_expr();

        static ASSIGNMENT_SYMBOLS: [OperatorSymbol; 5] =
            [PlusEq, TimesEq, MinusEq, DivideEq, Assign];

        let operator = match self.peek_token(1) {
            Some(Token::OperatorSymbol(op_symbol)) if ASSIGNMENT_SYMBOLS.contains(op_symbol) => {
                let operator = BinaryOperator::from(*op_symbol);
                self.skip_token();
                operator
            }
            _ => return left,
        };

        let right = self.parse_assignment_expr();

        Expr::BinExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn parse_logical_or_expr(&mut self) -> Expr {
        self.parse_logical_and_expr()
    }

    fn parse_logical_and_expr(&mut self) -> Expr {
        self.parse_comparison_expression()
    }

    fn parse_comparison_expression(&mut self) -> Expr {
        static COMPARISON_SYMBOLS: [OperatorSymbol; 6] = [
            EqualTo,
            NotEqualTo,
            LessThan,
            LessOrEqualTo,
            GreaterThan,
            GreaterOrEqualTo,
        ];

        let left = self.parse_add_sub_expr();

        let operator = match self.peek_token(1) {
            Some(Token::OperatorSymbol(op_symbol)) if COMPARISON_SYMBOLS.contains(op_symbol) => {
                let operator = BinaryOperator::from(*op_symbol);
                self.skip_token();
                operator
            }
            _ => return left,
        };

        let right = self.parse_add_sub_expr();

        if let Some(Token::OperatorSymbol(op_symbol)) = self.peek_token(1) {
            if COMPARISON_SYMBOLS.contains(op_symbol) {
                // TODO: print a useful error message for the user
                panic!("Comparison operators cannot be chained")
            }
        }

        Expr::BinExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn parse_add_sub_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_mul_div_expr();

        while let Some(Token::OperatorSymbol(op_symbol @ (Plus | Minus))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*op_symbol);
            self.skip_token();
            let right = self.parse_mul_div_expr();

            left_expr_so_far = Expr::BinExpr {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            }
        }

        left_expr_so_far
    }

    fn parse_mul_div_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_primary_expr();

        while let Some(Token::OperatorSymbol(op_symbol @ (Asterisk | Slash))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*op_symbol);
            self.skip_token();
            let right = self.parse_primary_expr();

            left_expr_so_far = Expr::BinExpr {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            }
        }

        left_expr_so_far
    }

    fn parse_primary_expr(&mut self) -> Expr {
        match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::LParen), _) => {
                self.skip_token();
                let expr = self.parse_expr();
                self.assert_next_token(Token::RParen);
                expr
            }
            (Some(Token::Identifier(_)), Some(Token::LParen | Token::LSquare)) => {
                self.parse_call_expr()
            }
            _ => self.parse_atom(),
        }
    }

    // f[5 + 3*9](3)[3](5)
    fn parse_call_expr(&mut self) -> Expr {
        let mut expr_so_far = self.parse_atom();

        while let Some(Token::LParen | Token::LSquare) = self.peek_token(1) {
            match self.peek_token(1).unwrap() {
                Token::LParen => {
                    expr_so_far = Expr::CallExpr {
                        function_name: Box::new(expr_so_far),
                        args: self.parse_args(),
                    }
                }
                Token::LSquare => {
                    expr_so_far = Expr::IndexExpr {
                        container: Box::new(expr_so_far),
                        index: self.parse_index(),
                    }
                }
                _ => unreachable!(),
            }
        }

        expr_so_far
    }

    fn parse_index(&mut self) {
        // 0:3:0, 3:3:3, 0:3, 5:8, :8
        todo!()
    }

    // f(,3,answer,test)
    // f()
    fn parse_args(&mut self) -> Vec<Expr> {
        self.assert_next_token(Token::LParen);

        let mut args = Vec::new();

        if let Some(Token::RParen) = self.peek_token(1) {
            return args;
        }

        loop {
            args.push(self.parse_expr());

            match self.next_token() {
                Some(Token::RParen) => break,
                Some(Token::Comma) => continue,
                _ => panic!("unexpected end of function arguments"),
            }
        }

        args
    }

    fn parse_atom(&mut self) -> Expr {
        match self.next_token() {
            Some(Token::Identifier(id)) => Expr::Identifier(id.clone()),
            Some(Token::IntLiteral(n)) => Expr::Int(*n),
            // todo Some(Token::StrLiteral())
            Some(token) => panic!("unexpected token {:?}, expected an atom", token),
            None => panic!("unexpected end of file, expected an atom"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast_parser::{ASTParser, BinaryOperator, Expr, Statement};
    use crate::lexer::Lexer;
    use crate::token::VarType;

    #[test]
    fn var_declaration() {
        let source_code = "int N = 5";
        let expected = vec![Statement::VarDeclaration {
            var_name: Expr::Identifier("N".to_string()),
            var_type: VarType::Int,
            var_value: Expr::Int(5),
        }];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn var_modification() {
        let source_code = "num = a = 10";
        let expected = vec![Statement::Expr(Expr::BinExpr {
            left: Box::new(Expr::Identifier("num".to_string())),
            operator: BinaryOperator::Assign,
            right: Box::new(Expr::BinExpr {
                left: Box::new(Expr::Identifier("a".to_string())),
                operator: BinaryOperator::Assign,
                right: Box::new(Expr::Int(10)),
            }),
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn empty_while_loop() {
        let source_code = "while i <= N {}";
        let expected = vec![Statement::WhileLoop {
            condition: Expr::BinExpr {
                left: Box::new(Expr::Identifier("i".to_string())),
                operator: BinaryOperator::LessOrEqualTo,
                right: Box::new(Expr::Identifier("N".to_string())),
            },
            body: vec![],
        }];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn order_of_operations() {
        let source_code = "10 + 3 * 8 / 4 - 13 + 5";
        let expected = vec![Statement::Expr(Expr::BinExpr {
            left: Box::new(Expr::BinExpr {
                left: Box::new(Expr::BinExpr {
                    left: Box::new(Expr::Int(10)),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::BinExpr {
                        left: Box::new(Expr::BinExpr {
                            left: Box::new(Expr::Int(3)),
                            operator: BinaryOperator::Multiply,
                            right: Box::new(Expr::Int(8)),
                        }),
                        operator: BinaryOperator::Divide,
                        right: Box::new(Expr::Int(4)),
                    }),
                }),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::Int(13)),
            }),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::Int(5)),
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn parenthetical_expression() {
        let source_code = "9*(2+3)";
        let expected = vec![Statement::Expr(Expr::BinExpr {
            left: Box::new(Expr::Int(9)),
            operator: BinaryOperator::Multiply,
            right: Box::new(Expr::BinExpr {
                left: Box::new(Expr::Int(2)),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::Int(3)),
            }),
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn spacing() {
        let source_code = "\n\n\n\t\t\ta\n\n";
        let expected = vec![Statement::Expr(Expr::Identifier("a".to_string()))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_call() {
        let source_code = "print(f(1)(2), 10, 20)";
        let expected = vec![Statement::Expr(Expr::CallExpr {
            function_name: Box::new(Expr::Identifier("print".to_string())),
            args: vec![
                Expr::CallExpr {
                    function_name: Box::new(Expr::CallExpr {
                        function_name: Box::new(Expr::Identifier("f".to_string())),
                        args: vec![Expr::Int(1)],
                    }),
                    args: vec![Expr::Int(2)],
                },
                Expr::Int(10),
                Expr::Int(20),
            ],
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = ASTParser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }
}
