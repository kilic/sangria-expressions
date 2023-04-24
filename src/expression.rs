use crate::{
    poly::{Polynomial, F},
    Instance,
};
use std::ops::{Add, Mul, Neg, Sub};

/// Sub-expression for values sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Variable {
    /// Witness value with index
    Value(usize),
}

/// Sub-expression for constant sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constant {
    /// Constant scalars
    Scalar(F),
    /// Fixed values with index
    Fixed(usize),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    // Sources
    Variable(Variable),
    Constant(Constant),
    // Relations
    Negated(Box<Expression>),
    Sum(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Scaled(Box<Expression>, F),
}

impl Expression {
    pub fn eval_poly(&self, fixed: &[Polynomial], current: &Instance) -> Polynomial {
        let size = current.error.len();
        (0..size)
            .map(|row_index| {
                self.eval(
                    &|a: F| -a,
                    &|a, b| a + b,
                    &|a, b| a * b,
                    &|a, b| a * b,
                    &|index| fixed[index][row_index],
                    &|scalar| scalar,
                    &|index| current.values[index][row_index],
                    &|index| current.seperators[index],
                )
            })
            .collect::<Vec<_>>()
            .into()
    }

    pub fn eval<T>(
        &self,
        // relations
        negated: &impl Fn(T) -> T,
        sum: &impl Fn(T, T) -> T,
        product: &impl Fn(T, T) -> T,
        scaled: &impl Fn(T, F) -> T,
        // constants
        fixed: &impl Fn(usize) -> T,
        scalar: &impl Fn(F) -> T,
        // valuess
        values: &impl Fn(usize) -> T,
        seperator: &impl Fn(usize) -> T,
    ) -> T {
        match self {
            Expression::Variable(var) => match var {
                Variable::Value(index) => values(*index),
            },
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => fixed(*index),
                Constant::Scalar(f) => scalar(*f),
            },
            Expression::Negated(a) => {
                let a = a.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                negated(a)
            }
            Expression::Sum(a, b) => {
                let a = a.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                let b = b.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                sum(a, b)
            }
            Expression::Product(a, b) => {
                let a = a.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                let b = b.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                product(a, b)
            }
            Expression::Scaled(a, f) => {
                let a = a.eval(
                    negated, sum, product, scaled, fixed, scalar, values, seperator,
                );
                scaled(a, *f)
            }
        }
    }

    pub(crate) fn folding_degree(&self) -> usize {
        match self {
            Expression::Variable(var) => match var {
                Variable::Value(_) => 1,
            },
            Expression::Constant(_) => 0,
            Expression::Negated(a) => a.folding_degree(),
            Expression::Sum(a, b) => std::cmp::max(a.folding_degree(), b.folding_degree()),
            Expression::Product(a, b) => a.folding_degree() + b.folding_degree(),
            Expression::Scaled(a, _) => a.folding_degree(),
        }
    }

    pub fn identifier(&self) -> String {
        let mut cursor = std::io::Cursor::new(Vec::new());
        self.write_identifier(&mut cursor).unwrap();
        String::from_utf8(cursor.into_inner()).unwrap()
    }

    fn write_identifier<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match self {
            Expression::Variable(var) => match var {
                Variable::Value(index) => write!(writer, "a{index}"),
            },
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => write!(writer, "q{index}"),
                Constant::Scalar(f) => write!(writer, "{f:?}"),
            },
            Expression::Negated(a) => {
                writer.write_all(b"( - ")?;
                a.write_identifier(writer)?;
                writer.write_all(b")")
            }
            Expression::Sum(a, b) => {
                writer.write_all(b"(")?;
                a.write_identifier(writer)?;
                writer.write_all(b" + ")?;
                b.write_identifier(writer)?;
                writer.write_all(b")")
            }
            Expression::Product(a, b) => {
                writer.write_all(b"(")?;
                a.write_identifier(writer)?;
                writer.write_all(b" * ")?;
                b.write_identifier(writer)?;
                writer.write_all(b")")
            }
            Expression::Scaled(a, f) => {
                a.write_identifier(writer)?;
                write!(writer, "* {f:?}")
            }
        }
    }
}

impl Neg for Expression {
    type Output = Expression;
    fn neg(self) -> Self::Output {
        Expression::Negated(Box::new(self))
    }
}

impl Add for Expression {
    type Output = Expression;
    fn add(self, rhs: Expression) -> Expression {
        if rhs == 0.into() {
            self
        } else if self == 0.into() {
            rhs
        } else {
            Expression::Sum(Box::new(self), Box::new(rhs))
        }
    }
}

impl Sub for Expression {
    type Output = Expression;
    fn sub(self, rhs: Expression) -> Expression {
        if rhs == 0.into() {
            self
        } else if self == 0.into() {
            -rhs
        } else {
            Expression::Sum(Box::new(self), Box::new(-rhs))
        }
    }
}

impl Mul for Expression {
    type Output = Expression;
    fn mul(self, rhs: Expression) -> Expression {
        // TODO: filter out 0s and 1s
        Expression::Product(Box::new(self), Box::new(rhs))
    }
}

impl Mul<F> for Expression {
    type Output = Expression;
    fn mul(self, rhs: F) -> Expression {
        // TODO: filter out 0s and 1s
        Expression::Scaled(Box::new(self), rhs)
    }
}

impl From<Constant> for Expression {
    fn from(constant: Constant) -> Self {
        Expression::Constant(constant)
    }
}

impl From<&Constant> for Expression {
    fn from(constant: &Constant) -> Self {
        Expression::Constant(constant.clone())
    }
}

impl From<usize> for Variable {
    fn from(index: usize) -> Self {
        Variable::Value(index)
    }
}

impl From<Variable> for Expression {
    fn from(var: Variable) -> Self {
        Expression::Variable(var.into())
    }
}

impl From<F> for Expression {
    fn from(c: F) -> Self {
        Expression::Constant(Constant::Scalar(c))
    }
}

impl From<&F> for Expression {
    fn from(c: &F) -> Self {
        Expression::Constant(Constant::Scalar(*c))
    }
}
