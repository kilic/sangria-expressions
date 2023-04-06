use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

use crate::{poly::F, AdviceQuery, Challenge, FixedQuery};

/// Sub-expression for variable sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Variable {
    /// Folding constant
    U(),
    /// Challenges
    Challenge(Challenge),
    /// Witnesses polynomials
    Advice(AdviceQuery),
    /// Gate seperator challenge
    Y(usize), // TODO: handle the case that first gate can be $y = 1$
}

/// Sub-expression for constant sources throughout folding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Constant {
    /// Constant scalars
    Scalar(F),
    /// Fixed polynomials
    Fixed(FixedQuery),
}

/// Sub-expression for folding variable sources
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Folding {
    /// Fresh variable instances
    Current(Variable),
    /// Running variable instances
    Running(Variable),
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

/// Single term with arbitrary number of variables and constants
/// `term = (if negated -1 else 1) * product(variables) * product(constants)`
#[derive(Clone, Debug)]
pub struct Term {
    pub variables: Vec<Variable>,
    pub constants: Vec<Constant>,
    pub negated: bool,
}

/// Flatenned expression as
/// `expr = term_1 + term_2 + ... + term_n`
#[derive(Clone, Debug)]
pub struct FlatExpression(Vec<Term>);

/// Residues of folding product contains products current and running variables including `u` and `y`
#[derive(Clone, Debug, Default)]
pub struct CrossTerm(Vec<Folding>);

/// Crossed term with levels of slack collection
#[derive(Clone, Debug)]
pub struct FoldingTerm {
    /// Residues of folding product indexed by level
    pub cross_terms: Vec<Vec<CrossTerm>>,
    /// Common constants for each cross level
    pub constants: Vec<Constant>,
    /// Negated flag
    pub negated: bool,
}

/// Folding flat expression
#[derive(Clone, Debug)]
pub struct FoldingExpression(Vec<FoldingTerm>);

impl Variable {
    fn current(&self) -> Folding {
        Folding::Current(self.clone())
    }
    fn running(&self) -> Folding {
        Folding::Running(self.clone())
    }
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

impl Term {
    /// Given the context of the expression generate cross terms
    pub fn cross(
        &self,
        max_variable_degree: usize,
        gate_index: usize,
        inject_seperator: bool,
    ) -> FoldingTerm {
        // recursively climb down the tree and collect combinations
        fn cross(
            // unfolded input variables
            variables: &Vec<Variable>,
            // folding variables for each degree
            cross_terms: &mut Vec<Vec<CrossTerm>>,
            // degree of recursion
            degree: usize,
            // running product of variables
            running: CrossTerm,
            // index of the folding product
            path: Vec<usize>,
        ) {
            let (running_left, running_right) = if degree == 0 {
                let running_left = CrossTerm(vec![variables[0].current()]);
                let running_rigth = CrossTerm(vec![variables[0].running()]);
                (running_left, running_rigth)
            } else {
                let mut running_left = running.clone();
                let mut running_rigth = running.clone();
                running_left.0.push(variables[degree].current());
                running_rigth.0.push(variables[degree].running());
                (running_left, running_rigth)
            };
            let degree = degree + 1;
            if degree == variables.len() {
                let left_index = path.iter().fold(0, |next, acc| acc + next);
                let right_index = left_index + 1;
                cross_terms[left_index].push(running_left);
                cross_terms[right_index].push(running_right);
            } else {
                let mut left_path = path.clone();
                let mut right_path = path.clone();
                left_path.push(0usize);
                right_path.push(1usize);
                cross(variables, cross_terms, degree, running_left, left_path);
                cross(variables, cross_terms, degree, running_right, right_path);
            }
        }
        assert!(self.variables.len() <= max_variable_degree);

        let mut variables = self.variables.clone();

        // inject folding terms `u` according to the degree
        for _ in 0..max_variable_degree - self.variables.len() {
            variables.push(Variable::U())
        }

        // inject `y` the gate seperator
        let mut cross_terms = if inject_seperator {
            variables.push(Variable::Y(gate_index));
            vec![vec![]; max_variable_degree + 2]
        } else {
            vec![vec![]; max_variable_degree + 1]
        };

        cross(
            &variables,
            &mut cross_terms,
            0,
            CrossTerm::default(),
            vec![],
        );

        // trim all-running that is (0,0,0, ...) and all-current (1,1,1, ...) cases
        // cross_terms.pop();
        // let cross_terms = cross_terms[1..].to_vec();

        FoldingTerm {
            cross_terms,
            constants: self.constants.clone(),
            negated: self.negated,
        }
    }
}

impl FlatExpression {
    pub fn max_variable_degree(&self) -> usize {
        let mut max_degree = 0;
        for term in self.0.iter() {
            max_degree = max_degree.max(term.variables.len());
        }
        max_degree
    }

    pub fn cross(
        &self,
        max_variable_degree: usize,
        gate_index: usize,
        inject_seperator: bool,
    ) -> FoldingExpression {
        assert!(self.max_variable_degree() <= max_variable_degree);
        let terms = self
            .0
            .iter()
            .map(|e| e.cross(max_variable_degree, gate_index, inject_seperator))
            .collect::<Vec<_>>();
        FoldingExpression(terms)
    }

    pub fn identifier(&self) -> String {
        let expr: Expression = self.into();
        expr.identifier()
    }
}

impl FoldingExpression {
    pub fn into_expressions(&self) -> Vec<Expression> {
        let number_of_levels = self.number_of_levels();
        (0..number_of_levels)
            .map(|i| {
                let expression = self
                    .0
                    .iter()
                    .map(|folding| {
                        let cross_terms = &folding.cross_terms[i];
                        let cross_terms = cross_terms
                            .iter()
                            .map(|e| e.into())
                            .collect::<Vec<Expression>>();
                        let cross_terms = Expression::sum(&cross_terms);
                        let constants = folding
                            .constants
                            .iter()
                            .map(|constant| constant.into())
                            .collect::<Vec<Expression>>();
                        let constants = Expression::product(&constants);
                        if folding.negated {
                            -constants * cross_terms
                        } else {
                            constants * cross_terms
                        }
                    })
                    .collect::<Vec<Expression>>();
                Expression::sum(&expression)
            })
            .collect()
    }

    fn number_of_levels(&self) -> usize {
        let number_of_levels = self.0[0].cross_terms.len();
        for folding in self.0.iter() {
            assert_eq!(folding.cross_terms.len(), number_of_levels);
        }
        number_of_levels
    }
}

impl Expression {
    pub fn scalar(e: F) -> Self {
        Constant::Scalar(e).into()
    }

    pub fn advice(e: AdviceQuery) -> Self {
        Variable::Advice(e).current().into()
    }

    pub fn challenge(index: usize, phase: u8) -> Self {
        Variable::Challenge(Challenge { index, phase })
            .current()
            .into()
    }

    pub fn fixed(e: FixedQuery) -> Self {
        Constant::Fixed(e).into()
    }

    pub fn evaluate<T>(
        &self,
        // relations
        negated: &impl Fn(T) -> T,
        sum: &impl Fn(T, T) -> T,
        product: &impl Fn(T, T) -> T,
        scaled: &impl Fn(T, F) -> T,
        // constants
        scalar: &impl Fn(F) -> T,
        fixed_column: &impl Fn(FixedQuery) -> T,
        // current variables
        u: &impl Fn() -> T,
        challenge: &impl Fn(Challenge) -> T,
        y: &impl Fn(usize) -> T,
        advice_column: &impl Fn(AdviceQuery) -> T,
        // running variables
        running_u: &impl Fn() -> T,
        running_challenge: &impl Fn(Challenge) -> T,
        running_y: &impl Fn(usize) -> T,
        running_advice_column: &impl Fn(AdviceQuery) -> T,
    ) -> T {
        match self {
            // variables
            Expression::Variable(folding) => match folding {
                Folding::Running(variable) => match variable {
                    Variable::U() => running_u(),
                    Variable::Y(idx) => running_y(*idx),
                    Variable::Challenge(value) => running_challenge(value.clone()),
                    Variable::Advice(query) => running_advice_column(query.clone()),
                },
                Folding::Current(variable) => match variable {
                    Variable::U() => u(),
                    Variable::Y(idx) => y(*idx),
                    Variable::Challenge(value) => challenge(value.clone()),
                    Variable::Advice(query) => advice_column(query.clone()),
                },
            },
            // constants
            Expression::Constant(constant) => match constant {
                Constant::Fixed(query) => fixed_column(query.clone()),
                Constant::Scalar(e) => scalar(e.clone()),
            },
            // relations
            Expression::Negated(a) => {
                let a = a.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                negated(a)
            }
            Expression::Sum(a, b) => {
                let a = a.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                let b = b.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                sum(a, b)
            }
            Expression::Product(a, b) => {
                let a = a.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                let b = b.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                product(a, b)
            }
            Expression::Scaled(a, f) => {
                let a = a.evaluate(
                    negated,
                    sum,
                    product,
                    scaled,
                    scalar,
                    fixed_column,
                    u,
                    challenge,
                    y,
                    advice_column,
                    running_u,
                    running_challenge,
                    running_y,
                    running_advice_column,
                );
                scaled(a, f.clone())
            }
        }
    }
    pub fn product(expressions: &Vec<Self>) -> Self {
        let mut expressions = expressions.clone();
        expressions.sort();
        if expressions.len() > 0 {
            expressions
                .iter()
                .skip(1)
                .fold(expressions[0].clone(), |acc, expr| expr.clone() * acc)
        } else {
            Expression::scalar(1)
        }
    }
    pub fn sum(expressions: &Vec<Self>) -> Self {
        let mut expressions = expressions.clone();
        expressions.sort();
        if expressions.len() > 0 {
            expressions
                .iter()
                .skip(1)
                .fold(expressions[0].clone(), |acc, expr| expr.clone() + acc)
        } else {
            Expression::scalar(0)
        }
    }
    fn flatten(&self) -> Vec<Self> {
        match &self {
            Expression::Sum(a, b) => a
                .flatten()
                .iter()
                .chain(b.flatten().iter())
                .cloned()
                .collect(),
            Expression::Product(a, b) => {
                let a = a.flatten();
                let b = b.flatten();
                a.iter()
                    .flat_map(|a_i| b.iter().map(|b_i| a_i.clone() * b_i.clone()))
                    .collect()
            }
            Expression::Scaled(poly, k) => poly
                .flatten()
                .iter()
                .map(|poly| poly.clone() * Expression::scalar(k.clone()))
                .collect(),
            _ => vec![self.clone()],
        }
    }
    fn variables(&self) -> Vec<Variable> {
        match self {
            // return variables
            Expression::Variable(folding) => match folding {
                Folding::Running(running) => unreachable!("expect current {:?}", running),
                Folding::Current(variable) => match variable {
                    Variable::U() => unreachable!("use before crossing"),
                    _ => vec![variable.clone()],
                },
            },
            // filter out constants
            Expression::Constant(_) => vec![],
            // recurse over sub-expressions
            Expression::Scaled(poly, _) => poly.variables(),
            Expression::Product(a, b) => a
                .variables()
                .iter()
                .chain(b.variables().iter())
                .cloned()
                .collect(),
            Expression::Negated(a) => a.variables(),
            Expression::Sum(_, _) => {
                unreachable!("single term with variable degree is expected")
            }
        }
    }

    fn constants(&self) -> Vec<Constant> {
        match self {
            // filter out variables
            Expression::Variable(_) => vec![],
            // return constants
            Expression::Constant(constant) => match constant {
                _ => vec![constant.clone()],
            },
            // recurse over sub-expressions
            Expression::Scaled(poly, _) => poly.constants(),
            Expression::Product(a, b) => a
                .constants()
                .iter()
                .chain(b.constants().iter())
                .cloned()
                .collect(),
            Expression::Negated(a) => a.constants(),
            Expression::Sum(_, _) => {
                unreachable!("single term with variable degree is expected")
            }
        }
    }

    pub fn is_negated(&self) -> bool {
        match self {
            // atomics
            Expression::Variable(_) => false,
            Expression::Constant(_) => false,
            // recurse over sub-expressions
            Expression::Scaled(poly, _) => poly.is_negated(),
            Expression::Product(a, b) => a.is_negated() ^ b.is_negated(),
            Expression::Negated(a) => true ^ a.is_negated(),
            Expression::Sum(_, _) => {
                unreachable!("single term with variable degree is expected")
            }
        }
    }

    pub fn identifier(&self) -> String {
        let mut cursor = std::io::Cursor::new(Vec::new());
        self.write_identifier(&mut cursor).unwrap();
        String::from_utf8(cursor.into_inner()).unwrap()
    }

    fn write_identifier<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match self {
            // variables
            Expression::Variable(folding) => match folding {
                Folding::Running(variable) => match variable {
                    Variable::U() => write!(writer, "u'"),
                    Variable::Y(idx) => write!(writer, "y'_{}", idx),
                    Variable::Challenge(challenge) => {
                        write!(writer, "∂'_{}_{}", challenge.index, challenge.phase)
                    }
                    Variable::Advice(query) => {
                        let name = query.name.as_str();
                        write!(writer, "{name}'[{:?}]", query.rotation.0)
                    }
                },
                Folding::Current(variable) => match variable {
                    Variable::U() => write!(writer, "u"),
                    Variable::Y(idx) => write!(writer, "y_{}", idx),
                    Variable::Challenge(challenge) => {
                        write!(writer, "∂_{}_{}", challenge.index, challenge.phase)
                    }
                    Variable::Advice(query) => {
                        let name = query.name.as_str();
                        write!(writer, "{name}[{:?}]", query.rotation.0)
                    }
                },
            },
            // constants
            Expression::Constant(constant) => match constant {
                Constant::Fixed(query) => {
                    let name = query.name.as_str();
                    write!(writer, "{name}[{:?}]", query.rotation.0)
                }
                Constant::Scalar(e) => write!(writer, "{:?}", *e),
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
                write!(writer, "* {:?}", f)
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
        if rhs == Self::scalar(0) {
            self
        } else if self == Self::scalar(0) {
            rhs
        } else {
            Expression::Sum(Box::new(self), Box::new(rhs))
        }
    }
}

impl Sub for Expression {
    type Output = Expression;
    fn sub(self, rhs: Expression) -> Expression {
        if rhs == Self::scalar(0) {
            self
        } else if self == Self::scalar(0) {
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
        Expression::Variable(folding.clone())
    }
}

impl From<&Folding> for Expression {
    fn from(folding: &Folding) -> Self {
        Expression::Variable(folding.clone())
    }
}

impl From<Constant> for Expression {
    fn from(constant: Constant) -> Self {
        Expression::Constant(constant.clone())
    }
}

impl From<&Constant> for Expression {
    fn from(constant: &Constant) -> Self {
        Expression::Constant(constant.clone())
    }
}

impl From<&Expression> for Term {
    fn from(expression: &Expression) -> Self {
        Self {
            variables: expression.variables(),
            constants: expression.constants(),
            negated: expression.is_negated(),
        }
    }
}

impl From<&Term> for Expression {
    fn from(term: &Term) -> Self {
        let constants: Vec<Expression> = term
            .constants
            .iter()
            .map(|constant| constant.into())
            .collect();
        let variables: Vec<Expression> = term
            .variables
            .iter()
            .map(|variables| variables.current().into())
            .collect();

        let expressions: Vec<Expression> =
            variables.into_iter().chain(constants.into_iter()).collect();
        let expression = Expression::product(&expressions);
        if term.negated {
            -expression
        } else {
            expression
        }
    }
}

impl From<&CrossTerm> for Expression {
    fn from(cross_term: &CrossTerm) -> Self {
        let cross_terms: Vec<Expression> =
            cross_term.0.iter().map(|folding| folding.into()).collect();
        Expression::product(&cross_terms)
    }
}

impl From<&Expression> for FlatExpression {
    fn from(expression: &Expression) -> Self {
        let terms = expression.flatten();
        let terms = terms.iter().map(|term| term.into()).collect::<Vec<Term>>();
        Self(terms)
    }
}

impl From<Expression> for FlatExpression {
    fn from(expression: Expression) -> Self {
        let terms = expression.flatten();
        let terms = terms.iter().map(|term| term.into()).collect::<Vec<Term>>();
        Self(terms)
    }
}

impl From<FlatExpression> for Expression {
    fn from(flat_expr: FlatExpression) -> Self {
        (&flat_expr).into()
    }
}

impl From<&FlatExpression> for Expression {
    fn from(flat_expr: &FlatExpression) -> Self {
        let terms = flat_expr
            .0
            .iter()
            .map(|term| term.into())
            .collect::<Vec<Expression>>();
        Expression::sum(&terms)
    }
}
