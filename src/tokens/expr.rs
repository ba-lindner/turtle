use std::fmt::{Display, Write as _};

use crate::{SymbolTable, pos::FilePos};

use super::{ArgList, Narrate, PredefFunc, ValType, Value, Variable};

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub start: FilePos,
    pub end: FilePos,
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Const(Value),
    Variable(Variable),
    BiOperation(Box<Expr>, BiOperator, Box<Expr>),
    UnOperation(UnOperator, Box<Expr>),
    Absolute(Box<Expr>),
    Bracket(Box<Expr>),
    Convert(Box<Expr>, ValType),
    FuncCall(PredefFunc, ArgList),
    CalcCall(usize, ArgList),
}

impl ExprKind {
    pub fn at(self, start: FilePos, end: FilePos) -> Expr {
        Expr {
            start,
            end,
            kind: self,
        }
    }
}

impl Narrate for Expr {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) {
        match &self.kind {
            ExprKind::Const(val) => {
                let _ = write!(buf, "{val}");
            }
            ExprKind::Variable(var) => var.narrate_buf(symbols, buf),
            ExprKind::BiOperation(lhs, op, rhs) => {
                lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, "{op}");
                rhs.narrate_buf(symbols, buf);
            }
            ExprKind::UnOperation(op, expr) => {
                let _ = write!(buf, "{op}");
                expr.narrate_buf(symbols, buf);
            }
            ExprKind::Absolute(expr) => {
                let _ = write!(buf, "|");
                expr.narrate_buf(symbols, buf);
                let _ = write!(buf, "|");
            }
            ExprKind::Bracket(expr) => {
                let _ = write!(buf, "(");
                expr.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            ExprKind::Convert(from, to) => {
                let _ = write!(buf, "{to}(");
                from.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            ExprKind::FuncCall(pdf, args) => {
                let _ = write!(buf, "{pdf}(");
                args.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            ExprKind::CalcCall(id, args) => {
                let _ = write!(
                    buf,
                    "{}(",
                    symbols
                        .get_index(*id)
                        .expect("missing calc in symbol table")
                        .0
                );
                args.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BiOperator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    UnEqual,
    And,
    Or,
}

impl BiOperator {
    pub fn types(&self) -> Vec<(ValType, ValType)> {
        match self {
            BiOperator::Add => vec![
                (ValType::Number, ValType::Number),
                (ValType::String, ValType::String),
            ],
            BiOperator::Sub | BiOperator::Mul | BiOperator::Div | BiOperator::Exp => {
                vec![(ValType::Number, ValType::Number)]
            }
            BiOperator::Less
            | BiOperator::LessEqual
            | BiOperator::Greater
            | BiOperator::GreaterEqual => vec![
                (ValType::Number, ValType::Boolean),
                (ValType::String, ValType::Boolean),
            ],
            BiOperator::Equal | BiOperator::UnEqual => vec![
                (ValType::Number, ValType::Boolean),
                (ValType::String, ValType::Boolean),
                (ValType::Boolean, ValType::Boolean),
            ],
            BiOperator::And | BiOperator::Or => vec![(ValType::Boolean, ValType::Boolean)],
        }
    }

    pub fn calc(&self, val1: f64, val2: f64) -> f64 {
        match self {
            Self::Add => val1 + val2,
            Self::Sub => val1 - val2,
            Self::Mul => val1 * val2,
            Self::Div => val1 / val2,
            Self::Exp => val1.powf(val2),
            _ => f64::NAN,
        }
    }

    pub fn eval(&self, lhs: &Value, rhs: &Value) -> Value {
        macro_rules! bi_op_helper {
            ($lhs:ident, $rhs:ident : {$($ty:ident => $res:expr),+ $(,)?}) => {
                match ($lhs, $rhs) {
                    $((Value::$ty($lhs), Value::$ty($rhs)) => {
                        $res.into()
                    })+
                    _ => panic!("operator {self} not defined for {}", lhs.val_type())
                }
            };
        }

        macro_rules! calc {
            ($op:tt) => {
                bi_op_helper!(lhs, rhs: {
                    Number => *lhs $op *rhs,
                })
            };
        }

        macro_rules! cmp {
            ($op:tt) => {
                bi_op_helper!(lhs, rhs: {
                    Number => lhs $op rhs,
                    String => lhs $op rhs,
                    Boolean => lhs $op rhs,
                })
            };
        }

        match self {
            BiOperator::Add => {
                bi_op_helper!(lhs, rhs: {
                    String => lhs.clone() + rhs,
                    Number => *lhs + *rhs,
                })
            }
            BiOperator::Sub => calc!(-),
            BiOperator::Mul => calc!(*),
            BiOperator::Div => calc!(/),
            BiOperator::Exp => {
                bi_op_helper!(lhs, rhs: {
                    Number => lhs.powf(*rhs),
                })
            }
            BiOperator::Less => cmp!(<),
            BiOperator::LessEqual => cmp!(<=),
            BiOperator::Greater => cmp!(>),
            BiOperator::GreaterEqual => cmp!(>=),
            BiOperator::Equal => cmp!(==),
            BiOperator::UnEqual => cmp!(!=),
            BiOperator::And => {
                bi_op_helper!(lhs, rhs: {
                    Boolean => *lhs && *rhs
                })
            }
            BiOperator::Or => {
                bi_op_helper!(lhs, rhs: {
                    Boolean => *lhs || *rhs
                })
            }
        }
    }
}

impl Display for BiOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BiOperator::Add => write!(f, "+"),
            BiOperator::Sub => write!(f, "-"),
            BiOperator::Mul => write!(f, "*"),
            BiOperator::Div => write!(f, "/"),
            BiOperator::Exp => write!(f, "^"),
            BiOperator::Less => write!(f, "<"),
            BiOperator::LessEqual => write!(f, "<="),
            BiOperator::Greater => write!(f, ">"),
            BiOperator::GreaterEqual => write!(f, ">="),
            BiOperator::Equal => write!(f, "=="),
            BiOperator::UnEqual => write!(f, "!="),
            BiOperator::And => write!(f, "&&"),
            BiOperator::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOperator {
    Negate,
    Not,
}

impl UnOperator {
    pub fn val_type(&self) -> ValType {
        match self {
            UnOperator::Negate => ValType::Number,
            UnOperator::Not => ValType::Boolean,
        }
    }

    pub fn eval(&self, val: &Value) -> Value {
        match (self, val) {
            (Self::Negate, Value::Number(n)) => Value::Number(-*n),
            (Self::Not, Value::Boolean(b)) => Value::Boolean(!*b),
            (op, val) => panic!("unary operator {op} not defined for {}", val.val_type()),
        }
    }
}

impl Display for UnOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOperator::Negate => write!(f, "-"),
            UnOperator::Not => write!(f, "!"),
        }
    }
}
