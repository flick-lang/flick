use crate::lexing::token::AssignmentSymbol::*;
use crate::lexing::token::OperatorSymbol::*;
use crate::lexing::token::Token;
use crate::parsing::ast::*;
use crate::types::Type;

/// A struct that takes tokens and parses them into a [abstract syntax tree](crate::parsing::ast)
pub struct Parser<'a> {
    /// The slice of tokens to parse
    tokens: &'a [Token],
    /// The index of the next unparsed token
    cursor: usize,
}

impl<'a> Parser<'a> {
    /// Returns a parsing instance ready to convert `tokens` into an [abstract syntax tree](crate::parsing::ast)
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

    /// Parses as many global statements as possible and returns a [Program] containing them all.
    pub fn parse_program(&mut self) -> Program {
        let mut global_statements = Vec::new();

        loop {
            self.skip_newlines_comments_and_docstrings();

            match self.parse_global_statement() {
                Some(s) => global_statements.push(s),
                None => break,
            }
        }

        Program { global_statements }
    }

    /// Parses a global statement, like an external function declaration or a function definition.
    fn parse_global_statement(&mut self) -> Option<GlobalStatement> {
        match self.peek_token(1) {
            Some(Token::Extern) => Some(GlobalStatement::Extern(self.parse_func_proto())),
            Some(Token::Fn | Token::Pub) => Some(GlobalStatement::FuncDef(self.parse_func_def())),
            Some(t) => panic!("Unknown global statement starting with token '{}'", t),
            None => None,
        }
    }

    /// Advances the cursor past all newline, comment, and docstring tokens.
    fn skip_newlines_comments_and_docstrings(&mut self) {
        // todo take into account the fact that docstring CAN appear in parse tree
        while let Some(Token::Newline | Token::Comment(_) | Token::Docstring(_)) = self.peek_token(1) {
            self.skip_token();
        }
    }

    /// Parses the `fn foo(i64 x) i64` part of a function definition or an external function
    /// declaration.
    fn parse_func_proto(&mut self) -> FuncProto {
        let func_visibility = match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::Pub), Some(Token::Fn)) => FuncVisibility::Public,
            (Some(Token::Pub), Some(t)) => panic!("Expected 'pub fn' but received {}", t),
            (Some(Token::Pub), None) => panic!("Expected 'pub fn' but file ended"),
            (Some(Token::Extern), Some(Token::Fn)) => FuncVisibility::Extern,
            (Some(Token::Extern), Some(t)) => panic!("Expected 'extern fn' but received {}", t),
            (Some(Token::Extern), None) => panic!("Expected 'extern fn' but file ended"),
            (Some(Token::Fn), _) => FuncVisibility::Private,
            (Some(t), _) => panic!("Expected 'fn' or 'pub' but received {:?}", t),
            (None, _) => panic!("Expected 'fn' or 'pub' but file ended"),
        };

        if func_visibility != FuncVisibility::Private {
            self.skip_token(); // skip the 'extern' / 'pub' in 'extern fn' / 'pub fn'
        }
        self.skip_token();  // skip the 'fn'

        let name = self.parse_identifier();
        let params = self.parse_func_params();

        let return_type = match self.peek_token(1) {
            Some(Token::LSquirly) => Type::Void,  // implicit void ret-type omitted before body opened
            Some(Token::Newline) => Type::Void,   // implicit void ret-type omitted but no '{' because, e.g., extern fn
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

        FuncProto {
            func_visibility,
            name,
            params,
            return_type: Box::new(return_type),
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
    /// fn foo(i64 x) i64 {
    ///     i64 a = 10;
    ///     ret a - x * 2;
    /// }
    /// ```
    fn parse_func_def(&mut self) -> FuncDef {
        let proto = self.parse_func_proto();
        let body = self.parse_body();
        FuncDef { proto, body }
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
                Some(token) => panic!("Expected ')' but received {}", token),
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
        let statement = match (self.peek_token(1)?, self.peek_token(2)) {
            (Token::Type(_), _) => Statement::VarDeclaration(self.parse_var_declaration()),
            (Token::While, _) => Statement::WhileLoop(self.parse_while_loop()),
            (Token::Fn, _) => panic!("Nested function definitions are not allowed"),
            (Token::Ret, _) => Statement::Return(self.parse_return_statement()),
            (Token::If, _) => Statement::If(self.parse_if_statement()),
            (Token::Identifier(_), Some(Token::AssignmentSymbol(_))) => {
                Statement::Assignment(self.parse_assignment())
            }
            (Token::Identifier(_), Some(Token::LParen)) => Statement::Call(self.parse_call()),
            (s, _) => panic!("Unexpected token to start statement: {}", s), // TODO: skip this line and keep checking the file for errors
        };

        match self.next_token() {
            Some(Token::Newline | Token::Comment(_) | Token::Docstring(_)) | None => Some(statement),
            Some(token) => panic!("Expected newline or EOF but received {}", token),
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
            Some(Token::Type(var_type)) => var_type.clone(),
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

    /// Parses 1 variable declaration
    ///
    /// # Flick example code
    /// - `i64 ten = 10`
    /// - `i64 hundred = 10 * ten`
    fn parse_var_declaration(&mut self) -> VarDeclaration {
        let var_type = self.parse_type();

        // TODO: Error messages: Add custom error message to tell user that variables can't be named the same as types
        //  (e.g. "void" or "i64")
        let var_name = self.parse_identifier();

        self.assert_next_token(Token::AssignmentSymbol(Eq));

        let var_value = self.parse_expr();

        VarDeclaration {
            var_name,
            var_type,
            var_value,
        }
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

        loop {
            self.skip_newlines_comments_and_docstrings();

            if self.peek_token(1) == Some(&Token::RSquirly) {
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

    /// Parses an if statement (`if [condition] [body]`) and panics if unsuccessful.
    ///
    /// See also: [Parser::parse_expr], [Parser::parse_body]
    ///
    /// # Flick example code
    /// ```text
    /// if i * i < p {
    ///     i += 1
    /// }
    fn parse_if_statement(&mut self) -> If {
        self.assert_next_token(Token::If);

        let condition = self.parse_expr();
        let then_body = self.parse_body();

        let else_body = match self.peek_token(1) {
            Some(&Token::Else) => Some(self.parse_else_statement()),
            _ => None
        };

        If { condition, then_body, else_body }
    }
    
    fn parse_else_statement(&mut self) -> Vec<Statement> {
        self.assert_next_token(Token::Else);

        match self.peek_token(1) {
            Some(Token::If) => vec![Statement::If(self.parse_if_statement())],
            Some(Token::LSquirly) => self.parse_body(),
            Some(t) => panic!("Unexpected token '{}' after 'else'; expected 'else {{' or 'else if'", t),
            None => panic!("Unexpected end of file after 'else'; expected 'else {{' or 'else if'")
        }
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
            Token::Newline => None,
            _ => Some(self.parse_expr()),
        }
    }

    /// Parses assignments like `a = b` or `_ = foo()`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_assignment(&mut self) -> Assignment {
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

        Assignment {
            name,
            value: Box::new(value),
        }
    }

    /// Parses an expression. Keep reading for order-of-operations details.
    ///
    /// # How does order of operations get handled?
    ///
    /// `parse_expr` calls `parse_logical_or_expr`, which calls `parse_logical_and_expr`, etc. Eventually,
    /// `parse_atom` will be called. Then, the next-tightest expression type will resume control, and so on.
    ///
    /// For example, when parsing `1 + 7 * 8`, `parse_add_sub_expr` (less deep) will call `parse_mul_div_expr`
    /// (more deep) to parse `1` and `7 * 8`,
    fn parse_expr(&mut self) -> Expr {
        self.parse_logical_or_expr()
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
            Some(Token::ComparatorSymbol(s)) => ComparisonOperator::from(*s),
            _ => return left,
        };

        self.skip_token(); // skip the compare symbol

        let right = self.parse_add_sub_expr();

        if let Some(Token::ComparatorSymbol(_)) = self.peek_token(1) {
            // TODO: Error messages: print a useful error message for the user
            panic!("Comparison operators cannot be chained")
        }

        Expr::Comparison(Comparison {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    /// Parses expressions like `A - B + C`;
    /// see [Parser::parse_expr] for expression-parsing details.
    fn parse_add_sub_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_mul_div_rem_expr();

        while let Some(Token::OperatorSymbol(s @ (Plus | Minus))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*s);
            self.skip_token();
            let right = self.parse_mul_div_rem_expr();

            left_expr_so_far = Expr::Binary(Binary {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
        }

        left_expr_so_far
    }

    /// Parses expressions like `A / B * C`.
    ///
    /// See [Parser::parse_expr] for expression-parsing details.
    fn parse_mul_div_rem_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_unary_expr();

        while let Some(Token::OperatorSymbol(s @ (Asterisk | Slash | Modulo))) = self.peek_token(1) {
            let operator = BinaryOperator::from(*s);
            self.skip_token();
            let right = self.parse_unary_expr();

            left_expr_so_far = Expr::Binary(Binary {
                left: Box::new(left_expr_so_far),
                operator,
                right: Box::new(right),
            })
        }

        left_expr_so_far
    }

    /// Parses expressions like `-A` or `(u32) B`
    fn parse_unary_expr(&mut self) -> Expr {
        match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::OperatorSymbol(Minus)), _) => Expr::Unary(self.parse_negation()),
            (Some(Token::LParen), Some(Token::Type(_))) => Expr::Unary(self.parse_cast()),
            _ => self.parse_primary_expr(),
        }
    }

    /// Parses negation expressions like `-A`.
    fn parse_negation(&mut self) -> Unary {
        self.assert_next_token(Token::OperatorSymbol(Minus));
        let operand = self.parse_unary_expr();

        Unary {
            operator: UnaryOperator::Negate,
            operand: Box::new(operand),
        }
    }

    /// Parses cast expressions like `(u32) A`.
    fn parse_cast(&mut self) -> Unary {
        self.assert_next_token(Token::LParen);
        let cast_type = self.parse_type();
        self.assert_next_token(Token::RParen);
        let operand = self.parse_unary_expr();

        Unary {
            operator: UnaryOperator::Cast(cast_type),
            operand: Box::new(operand),
        }
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
            (Some(Token::Identifier(_)), Some(Token::LParen)) => Expr::Call(self.parse_call()),
            _ => self.parse_atom(),
        }
    }

    /// Parses expressions like `foo()` or `bar(7, 2)`; see [Parser::parse_expr] for 
    /// expression-parsing details.
    fn parse_call(&mut self) -> Call {
        let function_name = self.parse_identifier();
        let args = self.parse_func_args();
        Call {
            function_name,
            args
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
        match (self.peek_token(1), self.peek_token(2)) {
            (Some(Token::Identifier(_)), _) => Expr::Identifier(self.parse_identifier()),
            (Some(Token::IntLiteral(_)), _) => Expr::IntLiteral(self.parse_int_literal()),
            (Some(Token::True | Token::False), _) => Expr::BoolLiteral(self.parse_bool_literal()),

            // todo Some(Token::StrLiteral())

            (Some(token), _) => panic!("Expected identifier or literal but received '{}'", token),
            (None, _) => panic!("Expected identifier or literal but file ended"),
        }
    }

    /// Parses an integer literal, considering if it's negative by checking for a minus sign.
    ///
    /// # Flick example code
    /// - `42`
    /// - `-42`
    fn parse_int_literal(&mut self) -> String {
        // The next token should be an integer literal.
        match self.next_token() {
            Some(Token::IntLiteral(n)) => n.clone(),
            _ => unreachable!("This function is called from parse_atom, which already checks the next token")
        }
    }

    fn parse_bool_literal(&mut self) -> bool {
        match self.next_token() {
            Some(Token::True) => true,
            Some(Token::False) => false,
            _ => unreachable!("This function is called from parse_atom, which already checks the next token")
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexing::token::ComparatorSymbol::LessOrEqualTo;
    use crate::types::IntType;

    #[test]
    fn var_declaration() {
        let tokens = vec![
            Token::Type(Type::Int(IntType { signed: true, width: 64 })),
            Token::Identifier("x".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::IntLiteral("5".to_string()),
        ];
        let expected = Some(Statement::VarDeclaration(VarDeclaration {
            var_name: "x".to_string(),
            var_type: Type::Int(IntType { signed: true, width: 64 }),
            var_value: Expr::IntLiteral("5".to_string()),
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn var_modification() {
        let tokens = vec![
            Token::Identifier("num".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::IntLiteral("10".to_string()),
        ];
        let expected = Some(Statement::Assignment(Assignment {
            name: "num".to_string(),
            value: Box::new(Expr::IntLiteral("10".to_string())),
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn empty_while_loop() {
        let tokens = vec![
            Token::While,
            Token::Identifier("i".to_string()),
            Token::ComparatorSymbol(LessOrEqualTo),
            Token::Identifier("N".to_string()),
            Token::LSquirly,
            Token::RSquirly,
        ];
        let expected = Some(Statement::WhileLoop(WhileLoop {
            condition: Expr::Comparison(Comparison {
                left: Box::new(Expr::Identifier("i".to_string())),
                operator: ComparisonOperator::LessOrEqualTo,
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
            Token::IntLiteral("10".to_string()),
            Token::OperatorSymbol(Plus),
            Token::IntLiteral("3".to_string()),
            Token::OperatorSymbol(Asterisk),
            Token::IntLiteral("8".to_string()),
            Token::OperatorSymbol(Slash),
            Token::IntLiteral("4".to_string()),
            Token::OperatorSymbol(Minus),
            Token::IntLiteral("13".to_string()),
            Token::OperatorSymbol(Plus),
            Token::IntLiteral("5".to_string()),
        ];
        let expected = Expr::Binary(Binary {
            left: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::IntLiteral("10".to_string())),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::Binary(Binary {
                        left: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::IntLiteral("3".to_string())),
                            operator: BinaryOperator::Multiply,
                            right: Box::new(Expr::IntLiteral("8".to_string())),
                        })),
                        operator: BinaryOperator::Divide,
                        right: Box::new(Expr::IntLiteral("4".to_string())),
                    })),
                })),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::IntLiteral("13".to_string())),
            })),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::IntLiteral("5".to_string())),
        });

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_expr();

        assert_eq!(expected, ast);
    }

    #[test]
    fn parenthetical_expression() {
        let tokens = vec![
            Token::IntLiteral("9".to_string()),
            Token::OperatorSymbol(Asterisk),
            Token::LParen,
            Token::IntLiteral("2".to_string()),
            Token::OperatorSymbol(Plus),
            Token::IntLiteral("3".to_string()),
            Token::RParen,
        ];
        let expected = Expr::Binary(Binary {
            left: Box::new(Expr::IntLiteral("9".to_string())),
            operator: BinaryOperator::Multiply,
            right: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::IntLiteral("2".to_string())),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::IntLiteral("3".to_string())),
            })),
        });

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_expr();

        assert_eq!(expected, ast);
    }

    #[test]
    fn spacing() {
        let tokens = vec![
            Token::LSquirly,
            Token::Newline,
            Token::Newline,
            Token::Newline,
            Token::Identifier("a".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::IntLiteral("2".to_string()),
            Token::Newline,
            Token::Newline,
            Token::RSquirly,
        ];
        let expected = vec![Statement::Assignment(Assignment {
            name: "a".to_string(),
            value: Box::new(Expr::IntLiteral("2".to_string())),
        })];

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_body();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_call() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LParen,
            Token::Identifier("f".to_string()),
            Token::LParen,
            Token::IntLiteral("1".to_string()),
            Token::RParen,
            Token::Comma,
            Token::IntLiteral("10".to_string()),
            Token::Comma,
            Token::IntLiteral("20".to_string()),
            Token::RParen,
        ];
        let expected = Expr::Call(Call {
            function_name: "print".to_string(),
            args: vec![
                Expr::Call(Call {
                    function_name: "f".to_string(),
                    args: vec![Expr::IntLiteral("1".to_string())],
                }),
                Expr::IntLiteral("10".to_string()),
                Expr::IntLiteral("20".to_string()),
            ],
        });

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_expr();

        assert_eq!(expected, ast);
    }

    #[test]
    fn function_definition() {
        let tokens = vec![
            Token::Pub,
            Token::Fn,
            Token::Identifier("test".to_string()),
            Token::LParen,
            Token::Type(Type::Int(IntType { signed: true, width: 64 })),
            Token::Identifier("a".to_string()),
            Token::RParen,
            Token::Type(Type::Int(IntType { signed: true, width: 64 })),
            Token::LSquirly,
            Token::RSquirly,
        ];
        let expected = Program {
            global_statements: vec![GlobalStatement::FuncDef(FuncDef {
                proto: FuncProto {
                    func_visibility: FuncVisibility::Public,
                    name: "test".to_string(),
                    params: vec![FuncParam {
                        param_type: Type::Int(IntType { signed: true, width: 64 }),
                        param_name: "a".to_string(),
                    }],
                    return_type: Box::new(Type::Int(IntType { signed: true, width: 64 })),
                },
                body: vec![],
            })],
        };

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_program();

        assert_eq!(expected, ast);
    }

    #[test]
    fn if_statement() {
        let tokens = vec![
            Token::If,
            Token::Identifier("x".to_string()),
            Token::ComparatorSymbol(LessOrEqualTo),
            Token::IntLiteral("5".to_string()),
            Token::LSquirly,
            Token::Ret,
            Token::Newline,
            Token::RSquirly,
            Token::Else,
            Token::If,
            Token::Identifier("x".to_string()),
            Token::ComparatorSymbol(LessOrEqualTo),
            Token::IntLiteral("10".to_string()),
            Token::LSquirly,
            Token::Ret,
            Token::Newline,
            Token::RSquirly,
            Token::Else,
            Token::LSquirly,
            Token::Ret,
            Token::Newline,
            Token::RSquirly,
        ];
        let expected = Some(Statement::If(If { 
            condition: Expr::Comparison(Comparison { 
                left: Box::new(Expr::Identifier("x".to_string())), 
                operator: ComparisonOperator::LessOrEqualTo, 
                right: Box::new(Expr::IntLiteral("5".to_string())) ,
            }), 
            then_body: vec![Statement::Return(None)], 
            else_body: Some(vec![
                Statement::If(If { 
                    condition: Expr::Comparison(Comparison { 
                        left: Box::new(Expr::Identifier("x".to_string())), 
                        operator: ComparisonOperator::LessOrEqualTo, 
                        right: Box::new(Expr::IntLiteral("10".to_string())) ,
                    }),
                    then_body: vec![Statement::Return(None)],
                    else_body: Some(vec![Statement::Return(None)]),
                })
            ]),
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn return_statement() {
        let tokens = vec![
            Token::Ret,
            Token::Identifier("x".to_string()),
            Token::OperatorSymbol(Plus),
            Token::IntLiteral("5".to_string()),
        ];
        let expected = Some(Statement::Return(Some(Expr::Binary(Binary {
            left: Box::new(Expr::Identifier("x".to_string())),
            operator: BinaryOperator::Add,
            right: Box::new(Expr::IntLiteral("5".to_string())),
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
            Token::IntLiteral("5".to_string()),
        ];
        let expected = Some(Statement::Assignment(Assignment {
            name: "x".to_string(),
            value: Box::new(Expr::Binary(Binary {
                left: Box::new(Expr::Identifier("x".to_string())),
                operator: BinaryOperator::Add,
                right: Box::new(Expr::IntLiteral("5".to_string())),
            })),
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn arithmetic() {
        // x=(a+3)/4*5%3*(-2)-2
        let tokens = vec![
            Token::Identifier("x".to_string()),
            Token::AssignmentSymbol(Eq),
            Token::LParen,
            Token::Identifier("a".to_string()),
            Token::OperatorSymbol(Plus),
            Token::IntLiteral("3".to_string()),
            Token::RParen,
            Token::OperatorSymbol(Slash),
            Token::IntLiteral("4".to_string()),
            Token::OperatorSymbol(Asterisk),
            Token::IntLiteral("5".to_string()),
            Token::OperatorSymbol(Modulo),
            Token::IntLiteral("3".to_string()),
            Token::OperatorSymbol(Asterisk),
            Token::LParen,
            Token::OperatorSymbol(Minus),
            Token::IntLiteral("2".to_string()),
            Token::RParen,
            Token::OperatorSymbol(Minus),
            Token::IntLiteral("2".to_string()),
        ];

        let expected = Some(Statement::Assignment(Assignment {
            name: "x".to_string(),
            value: Box::new(Expr::Binary(Binary { 
                left: Box::new(Expr::Binary(Binary { 
                    left: Box::new(Expr::Binary(Binary { 
                        left: Box::new(Expr::Binary(Binary { 
                            left: Box::new(Expr::Binary(Binary { 
                                left: Box::new(Expr::Binary(Binary { 
                                    left: Box::new(Expr::Identifier("a".to_string())), 
                                    operator: BinaryOperator::Add, 
                                    right: Box::new(Expr::IntLiteral("3".to_string())) 
                                })), 
                                operator: BinaryOperator::Divide, 
                                right: Box::new(Expr::IntLiteral("4".to_string())) 
                            })), 
                            operator: BinaryOperator::Multiply, 
                            right: Box::new(Expr::IntLiteral("5".to_string())) 
                        })), 
                        operator: BinaryOperator::Remainder, 
                        right: Box::new(Expr::IntLiteral("3".to_string())) 
                    })), 
                    operator: BinaryOperator::Multiply, 
                    right: Box::new(Expr::Unary(Unary {
                        operator: UnaryOperator::Negate, 
                        operand: Box::new(Expr::IntLiteral("2".to_string()))
                    }))
                })), 
                operator: BinaryOperator::Subtract, 
                right: Box::new(Expr::IntLiteral("2".to_string()))
            }))
        }));

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_statement();

        assert_eq!(expected, ast);
    }

    #[test]
    fn unary_cast_of_call() {
        let tokens = vec![
            Token::LParen,
            Token::Type(Type::Int(IntType { width: 64, signed: true })),
            Token::RParen,
            Token::Identifier("foo".to_string()),
            Token::LParen,
            Token::IntLiteral("1".to_string()),
            Token::RParen,
        ];

        let expected = Expr::Unary(Unary {
            operator: UnaryOperator::Cast(Type::Int(IntType { width: 64, signed: true })),
            operand: Box::new(Expr::Call(Call {
                function_name: "foo".to_string(),
                args: vec![Expr::IntLiteral("1".to_string())],
            })),
        });

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse_expr();

        assert_eq!(expected, ast);
    }
}
