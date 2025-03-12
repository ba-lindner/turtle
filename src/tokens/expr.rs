use std::{
    collections::HashMap,
    fmt::{Display, Write as _},
};

use crate::{pos::FilePos, prog::Prototype, SymbolTable, TurtleError};

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
            kind: self
        }
    }
}

impl Expr {
    pub fn expect_type(
        &mut self,
        ty: ValType,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(value) => {
                value.val_type().assert(ty).map_err(e_map)?;
                Ok((true, true))
            },
            ExprKind::Variable(var) => {
                var.expect_type(ty, locals, globals)?;
                Ok((true, true))
            }
            ExprKind::BiOperation(lhs, op, rhs) => {
                for (inp, out) in op.types() {
                    if out == ty {
                        let (lhs_ok, rhs_ok) = (
                            lhs.expect_type(inp, protos, globals, locals)?,
                            rhs.expect_type(inp, protos, globals, locals)?,
                        );
                        return Ok((lhs_ok.0 && rhs_ok.0, lhs_ok.1 && rhs_ok.1))
                    }
                }
                return Err(TypeError::BiOpWrongType(*op, ty)).map_err(e_map);
            }
            ExprKind::UnOperation(op, expr) => {
                if op.val_type() != ty {
                    return Err(TypeError::UnOpWrongType(*op, ty)).map_err(e_map);
                }
                expr.expect_type(ty, protos, globals, locals)
            }
            ExprKind::Absolute(expr) => {
                if ty != ValType::Number {
                    return Err(e_map(TypeError::AbsoluteValue(ty)));
                }
                expr.expect_type(ty, protos, globals, locals)
            }
            ExprKind::Bracket(expr) => expr.expect_type(ty, protos, globals, locals),
            ExprKind::Convert(expr, vt) => {
                vt.assert(ty).map_err(e_map)?;
                let (_, l, g) = expr.val_type(protos, globals, locals)?;
                Ok((l, g))
            }
            ExprKind::FuncCall(pdf, exprs) => {
                pdf.ret_type().assert(ty).map_err(e_map)?;
                let fargs = pdf.args();
                if fargs.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(fargs.len(), exprs.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for i in 0..fargs.len() {
                    let (l, g) = exprs[i].expect_type(fargs[i], protos, globals, locals)?;
                    loc &= l;
                    glob &= g;
                }
                Ok((loc, glob))
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = &protos[&*idx];
                proto.ret.expect("calc should have ret type").assert(ty).map_err(e_map)?;
                if proto.args.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(proto.args.len(), exprs.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for i in 0..proto.args.len() {
                    let (l, g) = exprs[i].expect_type(proto.args[i].1, protos, globals, locals)?;
                    loc &= l;
                    glob &= g;
                }
                Ok((loc, glob))
            }
        }
    }

    pub fn val_type(
        &mut self,
        protos: &HashMap<usize, Prototype>,
        globals: &mut HashMap<usize, ValType>,
        locals: &mut HashMap<usize, ValType>,
    ) -> Result<(ValType, bool, bool), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeErrorSpan(e, self.start, self.end);
        match &mut self.kind {
            ExprKind::Const(val) => Ok((val.val_type(), true, true)),
            ExprKind::Variable(var) => var.val_type(locals, globals),
            ExprKind::BiOperation(lhs, op, rhs) => {
                let (lhs_type, rhs_type) = (
                    lhs.val_type(protos, globals, locals)?,
                    rhs.val_type(protos, globals, locals)?,
                );
                if lhs_type.0 != rhs_type.0 {
                    Err(e_map(TypeError::BiOpDifferentTypes(*op, lhs_type.0, rhs_type.0)))
                } else {
                    for (inp, out) in op.types() {
                        if inp == lhs_type.0 {
                            return Ok((out, lhs_type.1 && rhs_type.1, lhs_type.2 && rhs_type.2));
                        }
                    }
                    Err(e_map(TypeError::BiOpWrongType(*op, lhs_type.0)))
                }
            }
            ExprKind::UnOperation(op, expr) => {
                let ty = expr.val_type(protos, globals, locals)?;
                if ty.0 == op.val_type() {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::UnOpWrongType(*op, ty.0)))
                }
            }
            ExprKind::Absolute(expr) => {
                let ty = expr.val_type(protos, globals, locals)?;
                if ty.0 == ValType::Number {
                    Ok(ty)
                } else {
                    Err(e_map(TypeError::AbsoluteValue(ty.0)))
                }
            }
            ExprKind::Bracket(expr) => expr.val_type(protos, globals, locals),
            ExprKind::Convert(from, to) => {
                let (_, loc, glob) = from.val_type(protos, globals, locals)?;
                Ok((*to, loc, glob))
            }
            ExprKind::FuncCall(pdf, args) => {
                let fargs = pdf.args();
                if fargs.len() != args.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(args.len(), fargs.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for (idx, arg) in args.iter_mut().enumerate() {
                    let arg_ty = arg.val_type(protos, globals, locals)?;
                    if fargs[idx] != arg_ty.0 {
                        return Err(e_map(TypeError::ArgWrongType(idx, arg_ty.0, fargs[idx])));
                    }
                    loc &= arg_ty.1;
                    glob &= arg_ty.2;
                }
                Ok((pdf.ret_type(), loc, glob))
            }
            ExprKind::CalcCall(idx, exprs) => {
                let proto = &protos[&*idx];
                if proto.args.len() != exprs.len() {
                    return Err(e_map(TypeError::ArgsWrongLength(exprs.len(), proto.args.len())));
                }
                let (mut loc, mut glob) = (true, true);
                for (idx, arg) in exprs.iter_mut().enumerate() {
                    let arg_ty = arg.val_type(protos, globals, locals)?;
                    if proto.args[idx].1 != arg_ty.0 {
                        return Err(e_map(TypeError::ArgWrongType(idx, arg_ty.0, proto.args[idx].1)));
                    }
                    loc &= arg_ty.1;
                    glob &= arg_ty.2;
                }
                Ok((proto.ret.expect("calc should have return type"), loc, glob))
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("different types for operator {0}: {1} != {2}")]
    BiOpDifferentTypes(BiOperator, ValType, ValType),
    #[error("operator {0} not defined for {1}")]
    BiOpWrongType(BiOperator, ValType),
    #[error("operator {0} not defined for {1}")]
    UnOpWrongType(UnOperator, ValType),
    #[error("absolute value not defined for {0}")]
    AbsoluteValue(ValType),
    #[error("wrong number of arguments: got {0}, expected {1}")]
    ArgsWrongLength(usize, usize),
    #[error("argument #{0} has type {1}, should have {2}")]
    ArgWrongType(usize, ValType, ValType),
    #[error("wrong type: got {0}, expected {1}")]
    WrongType(ValType, ValType),
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
