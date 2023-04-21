use crate::{
    poly::{Polynomial, F},
    Instance,
};
use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

/// Sub-expression for values sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Variable {
    /// Folding constant
    U(),
    /// Witness value with index
    Value(usize),
    /// Gate seperator challenge
    Seperator(usize), // TODO: handle the case that first gate can be $y = 1$
}

impl Variable {
    fn current(&self) -> Folding {
        Folding::Current(self.clone())
    }
    fn running(&self) -> Folding {
        Folding::Running(self.clone())
    }
}

/// Sub-expression for constant sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constant {
    /// Constant scalars
    Scalar(F),
    /// Fixed values with index
    Fixed(usize),
}

/// Sub-expression for folding values sources
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Folding {
    /// Fresh values instances
    Current(Variable),
    /// Running values instances
    Running(Variable),
}

impl Folding {
    fn var(&self) -> Variable {
        match self {
            Folding::Current(v) => v.clone(),
            Folding::Running(v) => v.clone(),
        }
    }
}

impl PartialOrd for Folding {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.var().partial_cmp(&other.var())
    }
}

impl Ord for Folding {
    fn cmp(&self, other: &Self) -> Ordering {
        self.var().cmp(&other.var())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    // Sources
    Variable(Folding),
    Constant(Constant),
    // Relations
    Negated(Box<Expression>),
    Sum(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Scaled(Box<Expression>, F),
}

impl Expression {
    pub fn eval_poly(
        &self,
        fixed: &[Polynomial],
        current: &Instance,
        running: &Instance,
    ) -> Polynomial {
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
                    &|| current.u,
                    &|index| current.seperators[index],
                    &|index| running.values[index][row_index],
                    &|| running.u,
                    &|index| running.seperators[index],
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
        u: &impl Fn() -> T,
        seperator: &impl Fn(usize) -> T,

        running_values: &impl Fn(usize) -> T,
        running_u: &impl Fn() -> T,
        running_seperator: &impl Fn(usize) -> T,
    ) -> T {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(index) => values(*index),
                    Variable::U() => u(),
                    Variable::Seperator(index) => seperator(*index),
                },
                Folding::Running(var) => match var {
                    Variable::Value(index) => running_values(*index),
                    Variable::U() => running_u(),
                    Variable::Seperator(index) => running_seperator(*index),
                },
            },
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => fixed(*index),
                Constant::Scalar(f) => scalar(*f),
            },
            Expression::Negated(a) => {
                let a = a.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                negated(a)
            }
            Expression::Sum(a, b) => {
                let a = a.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                let b = b.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                sum(a, b)
            }
            Expression::Product(a, b) => {
                let a = a.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                let b = b.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                product(a, b)
            }
            Expression::Scaled(a, f) => {
                let a = a.eval(
                    negated,
                    sum,
                    product,
                    scaled,
                    fixed,
                    scalar,
                    values,
                    u,
                    seperator,
                    running_values,
                    running_u,
                    running_seperator,
                );
                scaled(a, *f)
            }
        }
    }

    // pub(crate) fn product(expressions: &Vec<Self>) -> Self {
    //     let mut expressions = expressions.clone();
    //     expressions.sort();
    //     assert!(!expressions.is_empty());
    //     expressions
    //         .iter()
    //         .skip(1)
    //         .fold(expressions[0].clone(), |acc, expr| expr.clone() * acc)
    // }

    pub(crate) fn sum(expressions: &[Self]) -> Self {
        let mut expressions = expressions.to_vec();
        assert!(!expressions.is_empty());
        expressions.sort();
        expressions
            .iter()
            .skip(1)
            .fold(expressions[0].clone(), |acc, expr| expr.clone() + acc)
    }

    pub(crate) fn pow(&self, degree: usize) -> Self {
        assert!(degree > 0);
        (0..degree)
            .skip(1)
            .fold(self.clone(), |acc, _| acc * self.clone())
    }

    pub(crate) fn folding_degree(&self) -> usize {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(_) => 1,
                    Variable::U() => 1,
                    Variable::Seperator(_) => unreachable!(),
                },
                Folding::Running(_) => unreachable!(),
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
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(index) => write!(writer, "a{index}"),
                    Variable::U() => write!(writer, "u"),
                    Variable::Seperator(index) => write!(writer, "y{index}"),
                },
                Folding::Running(var) => match var {
                    Variable::Value(index) => write!(writer, "a'{index}"),
                    Variable::U() => write!(writer, "u'"),
                    Variable::Seperator(index) => write!(writer, "y'{index}"),
                },
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

    pub(crate) fn relax(&self) -> Self {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(index) => Variable::Value(*index).into(),
                    Variable::U() => Variable::U().into(),
                    Variable::Seperator(_) => unreachable!(),
                },
                Folding::Running(_) => unreachable!(),
            },

            Expression::Constant(constant) => Expression::Constant(constant.clone()),
            Expression::Negated(a) => -a.relax(),
            Expression::Sum(a, b) => {
                let degree_a = a.folding_degree();
                let degree_b = b.folding_degree();
                let dif = degree_a.abs_diff(degree_b);
                if dif == 0 {
                    Expression::Sum(Box::new(a.relax()), Box::new(b.relax()))
                } else {
                    let u: Expression = Variable::U().into();
                    let u_power = Box::new(u.pow(dif));
                    if degree_a > degree_b {
                        let b = Expression::Product(b.clone(), u_power);
                        Expression::Sum(Box::new(a.relax()), Box::new(b.relax()))
                    } else {
                        let a = Expression::Product(a.clone(), u_power);
                        Expression::Sum(Box::new(a.relax()), Box::new(b.relax()))
                    }
                }
            }
            Expression::Product(a, b) => {
                Expression::Product(Box::new(a.relax()), Box::new(b.relax()))
            }
            Expression::Scaled(a, factor) => Expression::Scaled(Box::new(a.relax()), *factor),
        }
    }

    pub(crate) fn cross(&self) -> Vec<Expression> {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(index) => vec![
                        Variable::Value(*index).current().into(),
                        Variable::Value(*index).running().into(),
                    ],
                    Variable::U() => vec![
                        Variable::U().current().into(),
                        Variable::U().running().into(),
                    ],
                    Variable::Seperator(_) => unreachable!(),
                },
                Folding::Running(_) => unreachable!(),
            },

            Expression::Constant(c) => vec![Expression::Constant(c.clone())],
            // Expression::Negated(a) => a.cross(),
            Expression::Negated(a) => {
                let crossed = a.cross();
                // TODO: hit this in expresion test!
                crossed.iter().map(|a| -a.clone()).collect()
            }
            Expression::Sum(a, b) => {
                let a = a.cross();
                let b = b.cross();
                assert_eq!(a.len(), b.len());
                a.iter()
                    .zip(b.iter())
                    .map(|(a, b)| a.clone() + b.clone())
                    .collect()
            }
            Expression::Product(a, b) => {
                let terms_a = a.cross();
                let terms_b = b.cross();

                let degree = terms_a.len() + terms_b.len() - 1;
                let mut terms: Vec<Expression> = Vec::with_capacity(degree);
                for (i, a) in terms_a.iter().enumerate() {
                    for (j, b) in terms_b.iter().enumerate() {
                        let index = i + j;
                        let expr = a.clone() * b.clone();
                        if index >= terms.len() {
                            terms.push(expr);
                        } else {
                            terms[index] = terms[index].clone() + expr;
                        }
                    }
                }
                terms
            }
            Expression::Scaled(a, factor) => {
                let mut terms = a.cross();
                terms.iter_mut().map(|a| a.clone() * *factor).collect()
            }
        }
    }

    // injects the gate sperator
    // a little eval optimization for `y_running * gate_cuurent == 0` and `y_current * gate_running == 0`
    pub(crate) fn cross_with_seperator(&self, gate_index: usize) -> Vec<Expression> {
        let crossed_current = self.cross();
        let crossed_running = crossed_current.clone();

        let seperator_current: Expression = Variable::Seperator(gate_index).current().into();
        let mut crossed_current = crossed_current
            .iter()
            .map(|expr| expr.clone() * seperator_current.clone())
            .collect::<Vec<_>>();
        crossed_current.push(0.into());

        let seperator_running: Expression = Variable::Seperator(gate_index).running().into();
        let mut crossed_running = crossed_running
            .iter()
            .map(|expr| expr.clone() * seperator_running.clone())
            .collect::<Vec<_>>();
        crossed_running.insert(0, 0.into());
        assert!(crossed_current.len() == crossed_running.len());

        crossed_current
            .iter()
            .zip(crossed_running.iter())
            .map(|(a, b)| a.clone() + b.clone())
            .collect()
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

impl From<Folding> for Expression {
    fn from(folding: Folding) -> Self {
        Expression::Variable(folding)
    }
}

impl From<&Folding> for Expression {
    fn from(folding: &Folding) -> Self {
        Expression::Variable(folding.clone())
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

impl From<Variable> for Folding {
    fn from(var: Variable) -> Self {
        var.current()
    }
}
