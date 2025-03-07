use std::fmt::{Display, Write as _};

use crate::{prog::parser::TypeError, SymbolTable, TProgram};

use super::{ArgList, Narrate, PredefFunc, ValType, Value, Variable};

#[derive(Debug, PartialEq)]
pub enum Expr {
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

impl Expr {
    pub fn val_type(&self, prog: &TProgram) -> Result<ValType, TypeError> {
        match self {
            Expr::Const(val) => Ok(val.val_type()),
            Expr::Variable(var) => Ok(var.val_type()),
            Expr::BiOperation(lhs, op, rhs) => {
                let (lhs_type, rhs_type) = (lhs.val_type(prog)?, rhs.val_type(prog)?);
                if lhs_type != rhs_type {
                    Err(TypeError::BiOpDifferentTypes(*op, lhs_type, rhs_type))
                } else {
                    for (inp, out) in op.types() {
                        if inp == lhs_type {
                            return Ok(out);
                        }
                    }
                    Err(TypeError::BiOpWrongType(*op, lhs_type))
                }
            }
            Expr::UnOperation(op, expr) => {
                let ty = expr.val_type(prog)?;
                if ty == op.val_type() {
                    Ok(ty)
                } else {
                    Err(TypeError::UnOpWrongType(*op, ty))
                }
            }
            Expr::Absolute(expr) => {
                let ty = expr.val_type(prog)?;
                if ty == ValType::Number {
                    Ok(ValType::Number)
                } else {
                    Err(TypeError::AbsoluteValue(ty))
                }
            }
            Expr::Bracket(expr) => expr.val_type(prog),
            Expr::Convert(from, to) => {
                from.val_type(prog)?;
                Ok(*to)
            }
            Expr::FuncCall(pdf, args) => {
                let fargs = pdf.args();
                if fargs.len() != args.len() {
                    return Err(TypeError::ArgsWrongLength(args.len(), fargs.len()));
                }
                for (idx, arg) in args.iter().enumerate() {
                    let arg_ty = arg.val_type(prog)?;
                    if fargs[idx] != arg_ty {
                        return Err(TypeError::ArgWrongType(idx, arg_ty, fargs[idx]));
                    }
                }
                Ok(pdf.ret_type())
            }
            Expr::CalcCall(id, exprs) => {
                let calc = prog.get_calc(*id).unwrap();
                if calc.args.len() != exprs.len() {
                    return Err(TypeError::ArgsWrongLength(exprs.len(), calc.args.len()));
                }
                for (idx, arg) in exprs.iter().enumerate() {
                    let arg_ty = arg.val_type(prog)?;
                    if calc.args[idx].1 != arg_ty {
                        return Err(TypeError::ArgWrongType(idx, arg_ty, calc.args[idx].1));
                    }
                }
                calc.ret.val_type(prog)
            }
        }
    }
}

impl Narrate for Expr {
    fn narrate_buf(&self, symbols: &SymbolTable, buf: &mut String) {
        match self {
            Expr::Const(val) => {
                let _ = write!(buf, "{val}");
            }
            Expr::Variable(var) => var.narrate_buf(symbols, buf),
            Expr::BiOperation(lhs, op, rhs) => {
                lhs.narrate_buf(symbols, buf);
                let _ = write!(buf, "{op}");
                rhs.narrate_buf(symbols, buf);
            }
            Expr::UnOperation(op, expr) => {
                let _ = write!(buf, "{op}");
                expr.narrate_buf(symbols, buf);
            }
            Expr::Absolute(expr) => {
                let _ = write!(buf, "|");
                expr.narrate_buf(symbols, buf);
                let _ = write!(buf, "|");
            }
            Expr::Bracket(expr) => {
                let _ = write!(buf, "(");
                expr.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            Expr::Convert(from, to) => {
                let _ = write!(buf, "{to}(");
                from.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            Expr::FuncCall(pdf, args) => {
                let _ = write!(buf, "{pdf}(");
                args.narrate_buf(symbols, buf);
                let _ = write!(buf, ")");
            }
            Expr::CalcCall(id, args) => {
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
            BiOperator::Add => vec![(ValType::Number, ValType::Number), (ValType::String, ValType::String)],
            BiOperator::Sub |
            BiOperator::Mul |
            BiOperator::Div |
            BiOperator::Exp => vec![(ValType::Number, ValType::Number)],
            BiOperator::Less |
            BiOperator::LessEqual |
            BiOperator::Greater |
            BiOperator::GreaterEqual => vec![(ValType::Number, ValType::Boolean), (ValType::String, ValType::Boolean)],
            BiOperator::Equal |
            BiOperator::UnEqual => vec![(ValType::Number, ValType::Boolean), (ValType::String, ValType::Boolean), (ValType::Boolean, ValType::Boolean)],
            BiOperator::And |
            BiOperator::Or => vec![(ValType::Boolean, ValType::Boolean)],
        }
    }

    pub fn calc(&self, val1: f64, val2: f64) -> f64 {
        match self {
            Self::Add => val1 + val2,
            Self::Sub => val1 - val2,
            Self::Mul => val1 * val2,
            Self::Div => val1 / val2,
            Self::Exp => val1.powf(val2),
            _ => f64::NAN
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
            (op, val) => panic!("unary operator {op} not defined for {}", val.val_type())
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