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
            None => unreachable!("unexpected end of file, expected {:?}", expected),
        }
    }

    fn parse_var_dec(&mut self) -> Statement {
        self.assert_next_token(Token::Var);

        let var_name = self.parse_primary_expr();

        self.assert_next_token(Token::Colon);

        let var_type = match self.next_token() {
            Some(Token::VarType(s)) => *s,
            Some(token) => unreachable!("unexpected token {:?}, expected identifier", token),
            None => unreachable!("unexpected end of file, expected identifier"),
        };

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
            body.push(self.parse_statement())
        }

        self.assert_next_token(Token::RSquirly);

        Statement::WhileLoop { condition, body }
    }

    fn parse_expr(&mut self) -> Expr {
        // todo maybe just have two cases for parenthetical / non parenthetical
        if let Some(Token::LParen) = self.peek_token(1) {
            self.skip_token();
            let expr = self.parse_assignment_expr();
            self.assert_next_token(Token::RParen);
            expr
        } else {
            self.parse_assignment_expr()
        }
    }

    /// E -> {X =} X | {X +=} X | {X -=} X | etc.
    //             // a + b + c
    //             // expr_so_far: a
    //             // +
    //             // next_operand: b
    //             // we want to make a+b the expr_so_far
    //             // then, next_operand: c    ->   expr_so_far: (a+b)  + c
    /// TODO: make this into a macro

    /// a = (b = (c = d))
    fn parse_assignment_expr(&mut self) -> Expr {
        let left = self.parse_logical_or_expr();

        static ASSIGNMENT_SYMBOLS: [OperatorSymbol; 5] =
            [PlusEq, TimesEq, MinusEq, DivideEq, Assign];

        if let Some(Token::OperatorSymbol(op_symbol)) = self.peek_token(1) {
            if ASSIGNMENT_SYMBOLS.contains(op_symbol) {
                let operator = BinaryOperator::from(*op_symbol);

                self.skip_token(); // skip peeked OperatorSymbol

                return Expr::BinExpr {
                    left: Box::new(left),
                    operator,
                    right: Box::new(self.parse_assignment_expr()),
                };
            }
        }

        return left;
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

        let operator = match self.next_token() {
            Some(Token::OperatorSymbol(op_symbol)) if COMPARISON_SYMBOLS.contains(op_symbol) => {
                BinaryOperator::from(*op_symbol)
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
            left_expr_so_far = Expr::BinExpr {
                left: Box::new(left_expr_so_far.clone()), // todo see if works without clone
                operator: BinaryOperator::from(*op_symbol),
                right: Box::new(self.parse_mul_div_expr()),
            }
        }

        left_expr_so_far
    }

    fn parse_mul_div_expr(&mut self) -> Expr {
        let mut left_expr_so_far = self.parse_primary_expr();

        while let Some(Token::OperatorSymbol(op_symbol @ (Asterisk | Slash))) = self.peek_token(1) {
            left_expr_so_far = Expr::BinExpr {
                left: Box::new(left_expr_so_far.clone()), // todo see if works without clone
                operator: BinaryOperator::from(*op_symbol),
                right: Box::new(self.parse_primary_expr()),
            }
        }

        left_expr_so_far
    }

    fn parse_primary_expr(&mut self) -> Expr {
        match self.peek_token(2) {
            Some(Token::LParen) => self.parse_call_expr(),
            // Some(Token::LSquare) => self.parse_index_expr(),
            _ => self.parse_atom(),
        }
    }

    fn parse_call_expr(&mut self) -> Expr {
        let function_name = self.parse_primary_expr();
        self.assert_next_token(Token::LParen);
        let args = self.parse_args();

        Expr::CallExpr {
            function_name: Box::new(function_name),
            args,
        }
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        while let Some(Token::Comma) = self.peek_token(1) {
            self.skip_token();
            args.push(self.parse_expr());
        }
        args
    }

    fn parse_atom(&mut self) -> Expr {
        match self.next_token() {
            Some(Token::Identifier(id)) => Expr::Identifier(id.clone()),
            Some(Token::IntLiteral(n)) => Expr::Int(*n),
            // todo Some(Token::StrLiteral())
            Some(token) => unreachable!("unexpected token {:?}, expected an atom", token),
            None => unreachable!("unexpected end of file, expected an atom"),
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
        let source_code = "var N: int = 5";
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
}
