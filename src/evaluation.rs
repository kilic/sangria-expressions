use crate::{
    calculation::{Calculation, CalculationInfo},
    expression::{Constant, Expression, Variable},
    poly::{Polynomial, F},
    Instance, ValueSource,
};

#[derive(Clone, Debug)]
pub struct Evaluator {
    pub(crate) constants: Vec<F>,
    pub(crate) calculations: Vec<CalculationInfo>,
    pub(crate) num_intermediates: usize,
    pub(crate) targets: Vec<ValueSource>,
    pub(crate) number_of_gates: usize,
}

impl Evaluator {
    pub fn from(gates: Vec<Expression>) -> Self {
        let mut evaluator = Evaluator::new();

        evaluator.add_gates(gates);

        // also works like
        // for gate in gates.into_iter() {
        //     evaluator.add_gates(vec![gate]);
        // }

        evaluator
    }

    pub fn add_gates(&mut self, gates: Vec<Expression>) {
        assert!(!gates.is_empty());
        gates.iter().for_each(|gate| {
            self.add_cross(gate);
        });
    }

    pub(crate) fn new() -> Self {
        Self {
            constants: vec![0, 1, 2],
            calculations: Vec::new(),
            num_intermediates: 0,
            targets: Vec::new(),
            number_of_gates: 0,
        }
    }

    pub(crate) fn targets(&self) -> Vec<usize> {
        self.targets
            .iter()
            .map(|intermediate| match *intermediate {
                ValueSource::Intermediate(target) => target,
                _ => unreachable!(),
            })
            .collect::<Vec<_>>()
    }

    pub fn intermediates(&self) -> Vec<F> {
        vec![0; self.num_intermediates]
    }

    fn add_constant(&mut self, constant: &F) -> ValueSource {
        let position = self.constants.iter().position(|&c| c == *constant);
        ValueSource::Scalar(match position {
            Some(pos) => pos,
            None => {
                self.constants.push(*constant);
                self.constants.len() - 1
            }
        })
    }

    fn add_calculation(&mut self, calculation: Calculation) -> ValueSource {
        let existing_calculation = self
            .calculations
            .iter()
            .find(|c| c.calculation == calculation);
        match existing_calculation {
            Some(existing_calculation) => {
                //
                ValueSource::Intermediate(existing_calculation.target)
            }
            None => {
                let target = self.num_intermediates;
                self.calculations.push(CalculationInfo {
                    calculation,
                    target,
                });
                self.num_intermediates += 1;
                ValueSource::Intermediate(target)
            }
        }
    }

    pub fn add_expression(&mut self, expr: &Expression) -> ValueSource {
        match expr {
            // variables
            Expression::Variable(variable) => match variable {
                Variable::Value(index) => {
                    self.add_calculation(Calculation::Store(ValueSource::Value(*index)))
                }
            },
            // constants
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => {
                    self.add_calculation(Calculation::Store(ValueSource::Fixed(*index)))
                }
                Constant::Scalar(e) => self.add_constant(e),
            },
            // relations
            Expression::Negated(a) => match &**a {
                Expression::Constant(constant) => match constant {
                    Constant::Scalar(scalar) => self.add_constant(&-scalar),
                    _ => {
                        let result_a = self.add_expression(a);
                        match result_a {
                            ValueSource::Scalar(0) => result_a,
                            _ => self.add_calculation(Calculation::Negate(result_a)),
                        }
                    }
                },
                _ => {
                    let result_a = self.add_expression(a);
                    match result_a {
                        ValueSource::Scalar(0) => result_a,
                        _ => self.add_calculation(Calculation::Negate(result_a)),
                    }
                }
            },
            Expression::Sum(a, b) => {
                // Undo subtraction stored as a + (-b) in expressions
                match &**b {
                    Expression::Negated(b_int) => {
                        let result_a = self.add_expression(a);
                        let result_b = self.add_expression(b_int);
                        if result_a == ValueSource::Scalar(0) {
                            self.add_calculation(Calculation::Negate(result_b))
                        } else if result_b == ValueSource::Scalar(0) {
                            result_a
                        } else {
                            self.add_calculation(Calculation::Sub(result_a, result_b))
                        }
                    }
                    _ => {
                        let result_a = self.add_expression(a);
                        let result_b = self.add_expression(b);
                        if result_a == ValueSource::Scalar(0) {
                            result_b
                        } else if result_b == ValueSource::Scalar(0) {
                            result_a
                        } else if result_a <= result_b {
                            self.add_calculation(Calculation::Add(result_a, result_b))
                        } else {
                            self.add_calculation(Calculation::Add(result_b, result_a))
                        }
                    }
                }
            }
            Expression::Product(a, b) => {
                let result_a = self.add_expression(a);
                let result_b = self.add_expression(b);
                if result_a == ValueSource::Scalar(0) || result_b == ValueSource::Scalar(0) {
                    ValueSource::Scalar(0)
                } else if result_a == ValueSource::Scalar(1) {
                    result_b
                } else if result_b == ValueSource::Scalar(1) {
                    result_a
                } else if result_a == ValueSource::Scalar(2) {
                    self.add_calculation(Calculation::Double(result_b))
                } else if result_b == ValueSource::Scalar(2) {
                    self.add_calculation(Calculation::Double(result_a))
                } else if result_a == result_b {
                    self.add_calculation(Calculation::Square(result_a))
                } else if result_a <= result_b {
                    self.add_calculation(Calculation::Mul(result_a, result_b))
                } else {
                    self.add_calculation(Calculation::Mul(result_b, result_a))
                }
            }
            Expression::Scaled(a, f) => {
                if *f == 0 {
                    ValueSource::Scalar(0)
                } else if *f == 1 {
                    self.add_expression(a)
                } else {
                    let cst = self.add_constant(f);
                    let result_a = self.add_expression(a);
                    self.add_calculation(Calculation::Mul(result_a, cst))
                }
            }
        }
    }

    pub fn add_cross(&mut self, expr: &Expression) {
        assert!(expr.folding_degree() > 0);

        fn raise_by(
            ev: &mut Evaluator,
            targets: Vec<ValueSource>,
            cur: ValueSource, // TODO: assert u or y
            run: ValueSource, // TODO: assert u or y
        ) -> Vec<ValueSource> {
            // raise with `e`
            let t0: Vec<_> = targets
                .iter()
                .map(|target| ev.add_calculation(Calculation::Mul(target.clone(), cur.clone())))
                .collect();
            // raise with `e'`
            let t_shift: Vec<_> = targets
                .iter()
                .map(|target| ev.add_calculation(Calculation::Mul(target.clone(), run.clone())))
                .collect();
            // first term
            let mut t = vec![t0[0].clone()];
            // intermediate terms
            for (t0, t_shift) in t0.iter().skip(1).zip(t_shift.iter()) {
                t.push(ev.add_calculation(Calculation::Add(t0.clone(), t_shift.clone())));
            }
            // final term
            t.push(t_shift.last().unwrap().clone());

            t
        }

        fn raise_and_combine(
            ev: &mut Evaluator,
            mut a: Vec<ValueSource>,
            mut b: Vec<ValueSource>,
        ) -> Vec<ValueSource> {
            let degree_a = a.len();
            let degree_b = b.len();
            let dif = degree_a.abs_diff(degree_b);

            if degree_a > degree_b {
                (0..dif).for_each(|_| {
                    b = raise_by(ev, b.clone(), ValueSource::U(), ValueSource::RunningU())
                });
            } else {
                (0..dif).for_each(|_| {
                    a = raise_by(ev, a.clone(), ValueSource::U(), ValueSource::RunningU())
                });
            };
            assert_eq!(a.len(), b.len());
            a.into_iter()
                .zip(b.into_iter())
                .map(|(a, b)| ev.add_calculation(Calculation::Add(a, b)))
                .collect()
        }

        pub(crate) fn add_cross(ev: &mut Evaluator, expr: &Expression) -> Vec<ValueSource> {
            match expr {
                Expression::Variable(variable) => match variable {
                    Variable::Value(index) => {
                        let cur =
                            ev.add_calculation(Calculation::Store(ValueSource::Value(*index)));
                        let run = ev
                            .add_calculation(Calculation::Store(ValueSource::RunningValue(*index)));
                        vec![cur, run]
                    }
                },
                Expression::Constant(constant) => match constant {
                    Constant::Fixed(index) => {
                        vec![ev.add_calculation(Calculation::Store(ValueSource::Fixed(*index)))]
                    }
                    Constant::Scalar(e) => vec![ev.add_constant(e)],
                },
                Expression::Negated(a) => {
                    let t = add_cross(ev, a);
                    t.iter()
                        .map(|v| ev.add_calculation(Calculation::Negate(v.clone())))
                        .collect()
                }
                Expression::Sum(a, b) => {
                    let degree_a = a.folding_degree();
                    let degree_b = b.folding_degree();
                    let dif = degree_a.abs_diff(degree_b);
                    let a = add_cross(ev, a);
                    let b = add_cross(ev, b);
                    assert_eq!(a.len().abs_diff(b.len()), dif);
                    raise_and_combine(ev, a, b)
                }
                Expression::Product(a, b) => {
                    let terms_a = add_cross(ev, a);
                    let terms_b = add_cross(ev, b);
                    let degree = terms_a.len() + terms_b.len() - 1;
                    let mut terms: Vec<ValueSource> = Vec::with_capacity(degree);
                    for (i, a) in terms_a.iter().enumerate() {
                        for (j, b) in terms_b.iter().enumerate() {
                            let index = i + j;
                            let cal = ev.add_calculation(Calculation::Mul(a.clone(), b.clone()));
                            if index >= terms.len() {
                                terms.push(cal);
                            } else {
                                terms[index] =
                                    ev.add_calculation(Calculation::Add(terms[index].clone(), cal));
                            }
                        }
                    }
                    terms
                }
                Expression::Scaled(a, f) => {
                    if *f == 0 {
                        vec![ValueSource::Scalar(0)]
                    } else if *f == 1 {
                        add_cross(ev, a)
                    } else {
                        let cst = ev.add_constant(f);
                        let result_a = add_cross(ev, a);
                        result_a
                            .iter()
                            .map(|v| ev.add_calculation(Calculation::Mul(v.clone(), cst.clone())))
                            .collect()
                    }
                }
            }
        }

        let targets_new = add_cross(self, expr);

        // raise by seperator
        let y_index = self.number_of_gates;
        let targets_new = raise_by(
            self,
            targets_new,
            ValueSource::Seperator(y_index),
            ValueSource::RunningSeperator(y_index),
        );

        self.number_of_gates += 1;

        if self.targets().is_empty() {
            self.targets = targets_new;
        } else {
            self.targets = raise_and_combine(self, targets_new, self.targets.clone());
        }
    }

    pub fn eval(
        &self,
        intermediates: &mut [F],
        row_index: usize,
        fixed: &[Polynomial],
        //
        current_instance: &Instance,
        running_instance: &Instance,
    ) -> Vec<F> {
        for info in self.calculations.iter() {
            intermediates[info.target] = info.calculation.eval(
                row_index,
                intermediates,
                &self.constants,
                fixed,
                current_instance,
                running_instance,
            );
        }

        let targets = self.targets();

        targets
            .into_iter()
            .map(|target| intermediates[target])
            .collect()
    }
}
