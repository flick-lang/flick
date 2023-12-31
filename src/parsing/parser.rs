use crate::lexing::token::AssignmentSymbol::*;
use crate::lexing::token::OperatorSymbol::*;
use crate::lexing::token::{Token, Type};
use crate::parsing::ast::*;

/// A struct that takes tokens and parses them into a [abstract syntax tree](crate::parsing::ast)
pub struct Parser<'a> {
    /// The slice of tokens to parse
    tokens: &'a [Token],
    /// The index of the next unparsed token
    cursor: usize,
}

impl<'a> Parser<'a> {
    /// Returns an parsing instance ready to convert `tokens` into an [abstract syntax tree](crate::parsing::ast)
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    /// Returns a reference to the next token and advances the cursor past it.
    fn next_token(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    /// Returns a reference to the `n`-th token out of the remaining tokens.
    ///
    /// Note: this function returns `None` if fewer than `n` tokens remain.
    ///
    /// This function doesn't affect the internal state of the lexer (i.e., it doesn't consume
    /// any characters / it doesn't advance the internal cursor)
    fn peek_token(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + (n - 1)) // n-1 to fix indexing
    }

    /// Advances the cursor past the next `n` tokens without returning anything.
    fn skip_token(&mut self) {
        self.cursor += 1;
    }

    /// Parses as many function definitions as possible and returns a [Program]
    /// containing them all.
    pub fn parse_program(&mut self) -> Program {
        let mut func_defs = Vec::new();
        while let Some(func_def) = self.parse_func_def() {
            func_defs.push(func_def);
        }
        Program { func_defs }
    }

    /// Advances the cursor past all newline, comment, and docstring tokens.
    fn skip_newlines_comments_and_docstrings(&mut self) {
        // todo take into account the fact that docstring CAN appear in parse tree
        // enjoy this beautiful formatting <3
        while let Some(Token::Newline | Token::Comment(_) | Token::Docstring(_)) =
            self.peek_token(1)
        {
            self.skip_token();
        }
    }

    // TODO: Split the first part into a parse_func_proto function?
    /// Parses the next function definition, skipping comments and newlines.
    ///
    /// For example, the following Flick code can be parsed by this function:
    ///
    /// ```text
    /// // file: test.fl
    ///
    /// fn foo(i64 x) {
    ///     i64 a = 10;
    ///     return a - x * 2;
    /// }
    /// ```
    fn parse_func_def(&mut self) -> Option<FuncDef> {
        self.skip_newlines_comments_and_docstrings();

        let is_public = match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::Pub), Some(Token::Fn)) => true,
            (Some(Token::Pub), Some(t)) => panic!("Expected 'fn' but received {:?}", t),
            (Some(Token::Pub), None) => panic!("Expected 'fn' but file ended"),
            (Some(Token::Fn), _) => false,
            (Some(t), _) => panic!("Expected 'fn' or 'pub' but received {:?}", t),
            (None, _) => return None,
        };

        if is_public {
            self.skip_token();
        }
        self.skip_token();

        let name = self.parse_identifier();
        let params = self.parse_func_params();

        let return_type = match self.peek_token(1) {
            Some(Token::LSquirly) => Type::Void,
            Some(Token::Type(_)) => self.parse_type(),
            Some(t) => panic!(
                "Expected return type for function '{}' but received {:?}",
                name, t
            ),
            None => panic!(
                "Expected return type for function '{}' but file ended",
                name
            ),
        };

        let body = self.parse_body();

        let func_proto = FuncProto {
            name,
            params,
            return_type,
        };

        let func_def = FuncDef {
            is_public,
            proto: func_proto,
            body,
        };

        Some(func_def)
    }

    /// Parses function parameters, which is useful when parsing a function definition.
    ///
    /// Note, this function is similar to [Parser::parse_func_args], but it expects types,
    /// since params refer to the things being passed during a function definition (not a function call).
    ///
    /// # Flick example code
    ///
    /// - `()` - function takes no parameters
    /// - `(i64 x, i64 y)` - function takes two parameters
    ///
    /// # Assumptions
    ///
    /// - The function parameters are wrapped in parentheses.
    fn parse_func_params(&mut self) -> Vec<FuncParam> {
        self.assert_next_token(Token::LParen);

        let mut params = Vec::new();

        if let Some(Token::RParen) = self.peek_token(1) {
            self.skip_token();
            return params;
        }

        loop {
            let param_type = self.parse_type();
            let param_name = self.parse_identifier();

            let func_param = FuncParam {
                param_type,
                param_name,
            };

            params.push(func_param);

            match self.next_token() {
                Some(Token::RParen) => break,
                Some(Token::Comma) => continue,
                Some(token) => panic!("Expected ')' but received {:?}", token),
                None => panic!("Expected ')' but file ended"),
            }
        }

        params
    }

    /// Parses the next statement, skipping comments and newlines.
    ///
    /// # Flick example code
    /// - `print(x)`
    /// - `i += 1`
    fn parse_statement(&mut self) -> Option<Statement> {
        self.skip_newlines_comments_and_docstrings();

        let statement = match self.peek_token(1)? {
            Token::Type(_) => Statement::VarDeclarations(self.parse_var_declarations()),
            Token::While => Statement::WhileLoop(self.parse_while_loop()),
            Token::Fn => panic!("Nested function definitions are not allowed"),
            Token::Ret => Statement::Return(self.parse_return_statement()),
            _ => Statement::Expr(self.parse_expr()),
        };

        match self.next_token() {
            Some(Token::Newline | Token::Semicolon) | None => Some(statement),
            Some(token) => panic!("Expected newline or EOF but received {:?}", token),
        }
    }

    /// Panics if the token stream ended or if the next token doesn't match `expected`.
    fn assert_next_token(&mut self, expected: Token) {
        match self.next_token() {
            Some(token) if *token == expected => (),
            Some(token) => panic!("Expected {:?} but received {:?}", expected, token),
            None => panic!("Expected {:?} but file ended", expected),
        }
    }

    /// Parses a built-in type, like [Type::Void], and panics if the next token isn't one.
    fn parse_type(&mut self) -> Type {
        match self.next_token() {
            Some(Token::Type(var_type)) => *var_type,
            Some(t) => panic!("Expected type of variable but received {:?}", t),
            None => panic!("Expected type of variable but file ended"),
        }
    }

    // TODO: Error messages: split this function into several for caller to be more precise
    /// Parses an identifier, like `foo` or `x`, and panics if the next token isn't one.
    fn parse_identifier(&mut self) -> String {
        match self.next_token() {
            // TODO: Can we somehow get rid of this clone -- I think we can, but we have a choice:
            //  1. either have each ast node own its strings (this is what we're doing) -> then we must clone
            //  2. ast has a lifetime that outlives the token stream, in which case each ast can store &'a str
            Some(Token::Identifier(id)) => id.clone(),
            Some(t) => panic!("Expected identifier but received {:?}", t),
            None => panic!("Expected identifier but received end of file"),
        }
    }

    /// Parses 1 or more variable declaration, and panics if the next tokens don't form one.
    ///
    /// # Flick example code
    /// - `i64 a`
    /// - `i64 ten = 10, i64 hundred = 10 * ten, i64 counter`
    fn parse_var_declarations(&mut self) -> Vec<VarDeclaration> {
        let var_type = self.parse_type();

        let mut var_declarations = Vec::new();

        loop {
            // TODO: Error messages: Add custom error message to tell user that variables can't be named the same as types
            //  (e.g. "void" or "i64")
            let var_name = self.parse_identifier();

            let var_value = match self.peek_token(1) {
                Some(Token::AssignmentSymbol(Eq)) => {
                    self.skip_token();
                    Some(self.parse_expr())
                }
                _ => None,
            };

            let var_dec = VarDeclaration {
                var_name,
                var_type,
                var_value,
            };

            var_declarations.push(var_dec);

            match self.peek_token(1) {
                Some(Token::Comma) => self.skip_token(),
                _ => break,
            }
        }

        var_declarations
    }

    /// Parses 0 or more statements surrounded by curly brackets, and panics if unsuccessful.
    ///
    /// # Flick example code
    /// - `{}`
    /// - `{ print(10) }`
    /// - Multi-line bodies are also allowed, of course:
    /// ```text
    /// {
    ///     i64 a = b + c
    ///     print(a)
    /// }
    /// ```
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

    /// Parses a while loop (`while [expr] [body]`) and panics if unsuccessful.
    ///
    /// See also: [Parser::parse_expr], [Parser::parse_body]
    ///
    /// # Flick example code
    /// ```text
    /// while i * i < p {
    ///     i += 1
    /// }
    fn parse_while_loop(&mut self) -> WhileLoop {
        self.assert_next_token(Token::While);

        let condition = self.parse_expr();
        let body = self.parse_body();

        WhileLoop { condition, body }
    }

    /// Parses a return statement (`return [expr]` or just `return`), and panics if unsuccessful.
    fn parse_return_statement(&mut self) -> Option<Expr> {
        self.assert_next_token(Token::Ret);

        match self.peek_token(1)? {
            Token::Newline | Token::Semicolon => None,
            _ => Some(self.parse_expr()),
        }
    }

    /// Parses an expression. Keep reading for order-of-operations details.
    ///
    /// # How does order of operations get handled?
    ///
    /// `parse_expr` calls `parse_assignment_expr`, which calls `parse_logical_or_expr`, etc. Eventually,
    /// `parse_atom` will be called. Then, the next-tightest expression type will resume control, and so on.
    ///
    /// For example, when parsing `1 + 7 * 8`, `parse_add_sub_expr` (less deep) will call `parse_mul_div_expr`
    /// (more deep) to parse `1` and `7 * 8`,
    fn parse_expr(&mut self) -> Expr {
        self.parse_assignment_expr()
    }

    /// Parses expressions like `L = R`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_assignment_expr(&mut self) -> Expr {
        match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::Identifier(_)), Some(Token::AssignmentSymbol(_))) => {}
            _ => return self.parse_logical_or_expr(),
        }

        let name = self.parse_identifier();
        let operator_symbol = self.next_token().unwrap();

        let name_expr = Expr::Identifier(name.clone());

        let value = match operator_symbol {
            Token::AssignmentSymbol(PlusEq) => Expr::Binary(Binary {
                left: Box::new(name_expr),
                operator: BinaryOperator::Add,
                right: Box::new(self.parse_expr()),
            }),
            Token::AssignmentSymbol(TimesEq) => Expr::Binary(Binary {
                left: Box::new(name_expr),
                operator: BinaryOperator::Multiply,
                right: Box::new(self.parse_expr()),
            }),
            Token::AssignmentSymbol(MinusEq) => Expr::Binary(Binary {
                left: Box::new(name_expr),
                operator: BinaryOperator::Subtract,
                right: Box::new(self.parse_expr()),
            }),
            Token::AssignmentSymbol(DivideEq) => Expr::Binary(Binary {
                left: Box::new(name_expr),
                operator: BinaryOperator::Divide,
                right: Box::new(self.parse_expr()),
            }),
            Token::AssignmentSymbol(Eq) => self.parse_expr(),
            _ => unreachable!(),
        };

        Expr::Assign(Assign {
            name,
            value: Box::new(value),
        })
    }

    // TODO: implement logical or (rn we don't parse it bc it's not even lexed)
    /// Parses expressions like `A or B or C`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_logical_or_expr(&mut self) -> Expr {
        self.parse_logical_and_expr()
    }

    // TODO: implement logical and (rn we don't parse it bc it's not even lexed)
    /// Parses expressions like `A and B and C`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_logical_and_expr(&mut self) -> Expr {
        self.parse_comparison_expression()
    }

    /// Parses expressions like `L < R`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_comparison_expression(&mut self) -> Expr {
        let left = self.parse_add_sub_expr();

        let operator = match self.peek_token(1) {
            Some(Token::ComparatorSymbol(s)) => BinaryOperator::from(*s),
            _ => return left,
        };

        self.skip_token(); // skip the compare symbol

        let right = self.parse_add_sub_expr();

        if let Some(Token::ComparatorSymbol(_)) = self.peek_token(1) {
            // TODO: Error messages: print a useful error message for the user
            panic!("Comparison operators cannot be chained")
        }

        Expr::Binary(Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    /// Parses expressions like `A - B + C`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_add_sub_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_mul_div_expr();

        while let Some(Token::OperatorSymbol(s @ (Plus | Minus))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*s);
            self.skip_token();
            let right = self.parse_mul_div_expr();

            left_expr_so_far = Expr::Binary(Binary {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
        }

        left_expr_so_far
    }

    /// Parses expressions like `A / B * C`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_mul_div_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_primary_expr();

        while let Some(Token::OperatorSymbol(s @ (Asterisk | Slash))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*s);
            self.skip_token();
            let right = self.parse_primary_expr();

            left_expr_so_far = Expr::Binary(Binary {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
        }

        left_expr_so_far
    }

    /// Parses expressions like `(A + B)` or `foo()` or `x`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_primary_expr(&mut self) -> Expr {
        match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::LParen), _) => {
                self.skip_token();
                let expr = self.parse_expr();
                self.assert_next_token(Token::RParen);
                expr
            }
            (Some(Token::Identifier(_)), Some(Token::LParen)) => self.parse_call_expr(),
            _ => self.parse_atom(),
        }
    }

    /// Parses expressions like `foo()` or `bar(7, 2)`
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_call_expr(&mut self) -> Expr {
        let identifier = self.parse_identifier();
        match self.peek_token(1) {
            Some(Token::LParen) => Expr::Call(Call {
                function_name: identifier,
                args: self.parse_func_args(),
            }),
            _ => Expr::Identifier(identifier),
        }
    }

    /// Parses function args, which is useful during a function call.
    ///
    /// Note, this function is similar to [Parser::parse_func_params], but it doesn't expect types,
    /// since args refer to the things being passed during a function call (not a function definition).
    ///
    /// # Flick example code
    /// - `()`
    /// - `(7, 2)`
    ///
    /// # Assumptions
    /// - The args are wrapped in parentheses and properly comma-separated.
    fn parse_func_args(&mut self) -> Vec<Expr> {
        self.assert_next_token(Token::LParen);

        let mut args = Vec::new();

        if let Some(Token::RParen) = self.peek_token(1) {
            self.skip_token();
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

    /// Parses the most atomic expressions (identifiers/literals) and panics if unsuccessful.
    ///
    /// # Flick example code
    /// - `foo`
    /// - `42`
    fn parse_atom(&mut self) -> Expr {
        match self.next_token() {
            Some(Token::Identifier(id)) => Expr::Identifier(id.clone()),
            Some(Token::I64Literal(n)) => Expr::I64Literal(*n),
            // todo Some(Token::StrLiteral())
            Some(token) => panic!("Expected identifier or literal but received {:?}", token),
            None => panic!("Expected identifier or literal but file ended"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexing::token::ComparatorSymbol::LessThanOrEqualTo;

    #[test]
    fn var_declaration() {
        let tokens = vec![
            Token::Type(Type::I64),
            Token::Identifier("x".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::I64Literal(5),
            Token::Comma,
            Token::Identifier("a".to_string()),
            Token::Comma,
            Token::Identifier("m".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::I64Literal(3),
        ];
        let expected = Some(Statement::VarDeclarations(vec![
            VarDeclaration {
                var_name: "x".to_string(),
                var_type: Type::I64,
                var_value: Some(Expr::I64Literal(5)),
            },
            VarDeclaration {
                var_name: "a".to_string(),
                var_type: Type::I64,
                var_value: None,
            },
            VarDeclaration {
                var_name: "m".to_string(),
                var_type: Type::I64,
                var_value: Some(Expr::I64Literal(3)),
            },
        ]));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn var_modification() {
        let tokens = vec![
            Token::Identifier("num".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::Identifier("a".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::I64Literal(10),
        ];
        let expected = Some(Statement::Expr(Expr::Assign(Assign {
            name: "num".to_string(),
            value: Box::new(Expr::Assign(Assign {
                name: "a".to_string(),
                value: Box::new(Expr::I64Literal(10)),
            })),
        })));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn empty_while_loop() {
        let tokens = vec![
            Token::While,
            Token::Identifier("i".to_string()),
            Token::ComparatorSymbol(LessThanOrEqualTo),
            Token::Identifier("N".to_string()),
            Token::LSquirly,
            Token::RSquirly,
        ];
        let expected = Some(Statement::WhileLoop(WhileLoop {
            condition: Expr::Binary(Binary {
                left: Box::new(Expr::Identifier("i".to_string())),
                operator: BinaryOperator::LessOrEqualTo,
                right: Box::new(Expr::Identifier("N".to_string())),
            }),
            body: vec![],
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn order_of_operations() {
        let tokens = vec![
            Token::I64Literal(10),
            Token::OperatorSymbol(Plus),
            Token::I64Literal(3),
            Token::OperatorSymbol(Asterisk),
            Token::I64Literal(8),
            Token::OperatorSymbol(Slash),
            Token::I64Literal(4),
            Token::OperatorSymbol(Minus),
            Token::I64Literal(13),
            Token::OperatorSymbol(Plus),
            Token::I64Literal(5),
        ];
        let expected = Some(Statement::Expr(Expr::Binary(Binary {
            left: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::I64Literal(10)),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::Binary(Binary {
                        left: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::I64Literal(3)),
                            operator: BinaryOperator::Multiply,
                            right: Box::new(Expr::I64Literal(8)),
                        })),
                        operator: BinaryOperator::Divide,
                        right: Box::new(Expr::I64Literal(4)),
                    })),
                })),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::I64Literal(13)),
            })),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::I64Literal(5)),
        })));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn parenthetical_expression() {
        let tokens = vec![
            Token::I64Literal(9),
            Token::OperatorSymbol(Asterisk),
            Token::LParen,
            Token::I64Literal(2),
            Token::OperatorSymbol(Plus),
            Token::I64Literal(3),
            Token::RParen,
        ];
        let expected = Some(Statement::Expr(Expr::Binary(Binary {
            left: Box::new(Expr::I64Literal(9)),
            operator: BinaryOperator::Multiply,
            right: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::I64Literal(2)),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::I64Literal(3)),
            })),
        })));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn spacing() {
        let tokens = vec![
            Token::Newline,
            Token::Newline,
            Token::Newline,
            Token::Identifier("a".to_string()),
            Token::Newline,
            Token::Newline,
        ];
        let expected = Some(Statement::Expr(Expr::Identifier("a".to_string())));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_call() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LParen,
            Token::Identifier("f".to_string()),
            Token::LParen,
            Token::I64Literal(1),
            Token::RParen,
            Token::Comma,
            Token::I64Literal(10),
            Token::Comma,
            Token::I64Literal(20),
            Token::RParen,
        ];
        let expected = Some(Statement::Expr(Expr::Call(Call {
            function_name: "print".to_string(),
            args: vec![
                Expr::Call(Call {
                    function_name: "f".to_string(),
                    args: vec![Expr::I64Literal(1)],
                }),
                Expr::I64Literal(10),
                Expr::I64Literal(20),
            ],
        })));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_definition() {
        let tokens = vec![
            Token::Pub,
            Token::Fn,
            Token::Identifier("test".to_string()),
            Token::LParen,
            Token::Type(Type::I64),
            Token::Identifier("a".to_string()),
            Token::RParen,
            Token::Type(Type::I64),
            Token::LSquirly,
            Token::RSquirly,
        ];
        let expected = Program {
            func_defs: vec![FuncDef {
                is_public: true,
                proto: FuncProto {
                    name: "test".to_string(),
                    params: vec![FuncParam {
                        param_type: Type::I64,
                        param_name: "a".to_string(),
                    }],
                    return_type: Type::I64,
                },
                body: vec![],
            }],
        };

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_program();

        assert_eq!(expected, ast);
    }

    #[test]
    fn return_statement() {
        let tokens = vec![
            Token::Ret,
            Token::Identifier("x".to_string()),
            Token::OperatorSymbol(Plus),
            Token::I64Literal(5),
        ];
        let expected = Some(Statement::Return(Some(Expr::Binary(Binary {
            left: Box::new(Expr::Identifier("x".to_string())),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::I64Literal(5)),
        }))));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn plus_eq() {
        let tokens = vec![
            Token::Identifier("x".to_string()),
            Token::AssignmentSymbol(PlusEq),
            Token::I64Literal(5),
        ];
        let expected = Some(Statement::Expr(Expr::Assign(Assign {
            name: "x".to_string(),
            value: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::Identifier("x".to_string())),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::I64Literal(5)),
            })),
        })));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }
}
