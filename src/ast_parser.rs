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
    BinExpr {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
    },
    Int(isize),
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
    cursor: usize,  //
}

impl<'a> ASTParser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            cursor: 0
        }
    }

    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    /// Returns a reference to the next() value without advancing the cursor.
    fn peek(&mut self, n: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + (n-1)) // n-1 to fix indexing
    }
}

#[cfg(test)]
mod tests {
    use crate::ast_parser::{ASTParser, Expr, Statement};
    use crate::lexer::Lexer;
    use crate::token::VarType;

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
}