use crate::ast::{
    BinExpr, BinaryOperator, CallExpr, Expr, FuncDef, FuncParam, IndexExpr, Statement,
    VarDeclaration, WhileLoop,
};
use crate::token::OperatorSymbol::*;
use crate::token::Token;
use crate::token::{OperatorSymbol, Type};

// TODO(tbreydo): get rid of Parser object (just use functions)
pub struct Parser<'a> {
    tokens: &'a [Token],
    cursor: usize,
}

impl<'a> Parser<'a> {
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
            Token::Type(_) => Statement::VarDeclaration(self.parse_var_dec()),
            Token::While => Statement::WhileLoop(self.parse_while_loop()),
            Token::Fn => Statement::FuncDef(self.parse_func_definition()),
            _ => Statement::Expr(self.parse_expr()),
        };

        match self.next_token() {
            Some(Token::Newline) | None => Some(statement),
            Some(token) => panic!("Expected newline or EOF but received {:?}", token),
        }
    }

    // TODO: Maybe make this a macro?
    fn assert_next_token(&mut self, expected: Token) {
        match self.next_token() {
            Some(token) if *token == expected => (),
            Some(token) => panic!("Expected {:?} but received {:?}", expected, token),
            None => panic!("Expected {:?} but file ended", expected),
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.next_token() {
            Some(Token::Type(var_type)) => *var_type,
            Some(t) => panic!("Expected type of variable but received {:?}", t),
            None => panic!("Expected type of variable but file ended"),
        }
    }

    fn parse_identifier(&mut self) -> String {
        match self.next_token() {
            Some(Token::Identifier(id)) => id.clone(), // TODO: Can we somehow get rid of this clone
            Some(t) => panic!("Expected identifier but received {:?}", t),
            None => panic!("Expected identifier but received end of file"),
        }
    }

    fn parse_var_dec(&mut self) -> VarDeclaration {
        let var_type = self.parse_type();

        let var_name = self.parse_identifier();

        self.assert_next_token(Token::OperatorSymbol(Assign));

        let var_value = self.parse_expr();

        VarDeclaration {
            var_name,
            var_type,
            var_value,
        }
    }

    fn parse_body(&mut self) -> Vec<Statement> {
        let mut body = Vec::new();
        self.assert_next_token(Token::LSquirly);

        while let Some(token) = self.peek_token(1) {
            if *token == Token::RSquirly {
                break;
            }

            match self.parse_statement() {
                Some(statement) => body.push(statement),
                None => panic!("Expected body to be closed ('}}') but file ended"),
            }
        }
        self.assert_next_token(Token::RSquirly);
        body
    }

    fn parse_while_loop(&mut self) -> WhileLoop {
        self.assert_next_token(Token::While);

        let condition = self.parse_expr();
        let mut body = self.parse_body();

        WhileLoop { condition, body }
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

        Expr::BinExpr(BinExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
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

        Expr::BinExpr(BinExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_add_sub_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_mul_div_expr();

        while let Some(Token::OperatorSymbol(op_symbol @ (Plus | Minus))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*op_symbol);
            self.skip_token();
            let right = self.parse_mul_div_expr();

            left_expr_so_far = Expr::BinExpr(BinExpr {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
        }

        left_expr_so_far
    }

    fn parse_mul_div_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_primary_expr();

        while let Some(Token::OperatorSymbol(op_symbol @ (Asterisk | Slash))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*op_symbol);
            self.skip_token();
            let right = self.parse_primary_expr();

            left_expr_so_far = Expr::BinExpr(BinExpr {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
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
                    expr_so_far = Expr::CallExpr(CallExpr {
                        function_name: Box::new(expr_so_far),
                        args: self.parse_args(),
                    })
                }
                Token::LSquare => {
                    expr_so_far = Expr::IndexExpr(IndexExpr {
                        container: Box::new(expr_so_far),
                        index: self.parse_index(),
                    })
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
                Some(token) => panic!("Expected ')' but received {:?}", token),
                None => panic!("Expected ')' but file ended"),
            }
        }

        args
    }

    fn parse_atom(&mut self) -> Expr {
        match self.next_token() {
            Some(Token::Identifier(id)) => Expr::Identifier(id.clone()),
            Some(Token::IntLiteral(n)) => Expr::Int(*n),
            // todo Some(Token::StrLiteral())
            Some(token) => panic!("Expected identifier or literal but received {:?}", token),
            None => panic!("Expected identifier or literal but file ended"),
        }
    }

    fn parse_func_definition(&mut self) -> FuncDef {
        self.assert_next_token(Token::Fn);

        let name = self.parse_identifier();
        let params = self.parse_func_params();

        // fn test() blahblajdlskjlasdjf {
        let return_type = match self.peek_token(1) {
            Some(Token::LSquirly) => Type::Void,
            // Some(Token::Type(t)) => t,
            // _ => unreachable!("Expected return type for function {}", name,)
            _ => self.parse_type(),
        };

        let body = self.parse_body();

        FuncDef {
            name,
            params,
            return_type,
            body,
        }
    }

    fn parse_func_params(&mut self) -> Vec<FuncParam> {
        self.assert_next_token(Token::LParen);

        let mut params = Vec::new();

        if let Some(Token::RParen) = self.peek_token(1) {
            return params;
        }

        loop {
            params.push(self.parse_func_param());

            match self.next_token() {
                Some(Token::RParen) => break,
                Some(Token::Comma) => continue,
                Some(token) => panic!("Expected ')' but received {:?}", token),
                None => panic!("Expected ')' but file ended"),
            }
        }

        params
    }

    fn parse_func_param(&mut self) -> FuncParam {
        let param_name = self.parse_identifier();
        let param_type = self.parse_type();

        FuncParam {
            param_name,
            param_type,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{BinExpr, CallExpr, VarDeclaration, WhileLoop};
    use crate::lexer::Lexer;
    use crate::parser::{BinaryOperator, Expr, Parser, Statement};
    use crate::token::Type;

    #[test]
    fn var_declaration() {
        let source_code = "int N = 5";
        let expected = vec![Statement::VarDeclaration(VarDeclaration {
            var_name: "N".to_string(),
            var_type: Type::Int,
            var_value: Expr::Int(5),
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn var_modification() {
        let source_code = "num = a = 10";
        let expected = vec![Statement::Expr(Expr::BinExpr(BinExpr {
            left: Box::new(Expr::Identifier("num".to_string())),
            operator: BinaryOperator::Assign,
            right: Box::new(Expr::BinExpr(BinExpr {
                left: Box::new(Expr::Identifier("a".to_string())),
                operator: BinaryOperator::Assign,
                right: Box::new(Expr::Int(10)),
            })),
        }))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn empty_while_loop() {
        let source_code = "while i <= N {}";
        let expected = vec![Statement::WhileLoop(WhileLoop {
            condition: Expr::BinExpr(BinExpr {
                left: Box::new(Expr::Identifier("i".to_string())),
                operator: BinaryOperator::LessOrEqualTo,
                right: Box::new(Expr::Identifier("N".to_string())),
            }),
            body: vec![],
        })];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn order_of_operations() {
        let source_code = "10 + 3 * 8 / 4 - 13 + 5";
        let expected = vec![Statement::Expr(Expr::BinExpr(BinExpr {
            left: Box::new(Expr::BinExpr(BinExpr {
                left: Box::new(Expr::BinExpr(BinExpr {
                    left: Box::new(Expr::Int(10)),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::BinExpr(BinExpr {
                        left: Box::new(Expr::BinExpr(BinExpr {
                            left: Box::new(Expr::Int(3)),
                            operator: BinaryOperator::Multiply,
                            right: Box::new(Expr::Int(8)),
                        })),
                        operator: BinaryOperator::Divide,
                        right: Box::new(Expr::Int(4)),
                    })),
                })),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::Int(13)),
            })),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::Int(5)),
        }))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn parenthetical_expression() {
        let source_code = "9*(2+3)";
        let expected = vec![Statement::Expr(Expr::BinExpr(BinExpr {
            left: Box::new(Expr::Int(9)),
            operator: BinaryOperator::Multiply,
            right: Box::new(Expr::BinExpr(BinExpr {
                left: Box::new(Expr::Int(2)),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::Int(3)),
            })),
        }))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
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

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_call() {
        let source_code = "print(f(1)(2), 10, 20)";
        let expected = vec![Statement::Expr(Expr::CallExpr(CallExpr {
            function_name: Box::new(Expr::Identifier("print".to_string())),
            args: vec![
                Expr::CallExpr(CallExpr {
                    function_name: Box::new(Expr::CallExpr(CallExpr {
                        function_name: Box::new(Expr::Identifier("f".to_string())),
                        args: vec![Expr::Int(1)],
                    })),
                    args: vec![Expr::Int(2)],
                }),
                Expr::Int(10),
                Expr::Int(20),
            ],
        }))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_call() {
        let source_code = "fn test(a: int) int {}";
        let expected = vec![Statement::Expr(Expr::CallExpr(CallExpr {
            function_name: Box::new(Expr::Identifier("print".to_string())),
            args: vec![
                Expr::CallExpr(CallExpr {
                    function_name: Box::new(Expr::CallExpr(CallExpr {
                        function_name: Box::new(Expr::Identifier("f".to_string())),
                        args: vec![Expr::Int(1)],
                    })),
                    args: vec![Expr::Int(2)],
                }),
                Expr::Int(10),
                Expr::Int(20),
            ],
        }))];

        let source_code_chars: Vec<_> = source_code.chars().collect();
        let lexer = Lexer::new(&source_code_chars);
        let tokens: Vec<_> = lexer.collect();

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();

        assert_eq!(expected, ast);
    }
}
