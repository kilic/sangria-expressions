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
        let mut gates = gates;
        assert!(!gates.is_empty());
        gates.iter_mut().enumerate().for_each(|(index, gate)| {
            let y: Expression = Variable::Seperator(index + self.number_of_gates).into();
            *gate = gate.clone() * y;
        });
        let isolated = Expression::sum(&gates[..]);
        self.number_of_gates += gates.len();
        self.add_cross(&isolated);
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
                Variable::Seperator(index) => {
                    self.add_calculation(Calculation::Store(ValueSource::Seperator(*index)))
                }

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

    fn raise_by_u(&mut self, targets: Vec<ValueSource>) -> Vec<ValueSource> {
        // raise with `u`
        let t0: Vec<_> = targets
            .iter()
            .map(|target| self.add_calculation(Calculation::Mul(target.clone(), ValueSource::U())))
            .collect();

        // raise with `u'`
        let t_shift: Vec<_> = targets
            .iter()
            .map(|target| {
                self.add_calculation(Calculation::Mul(target.clone(), ValueSource::RunningU()))
            })
            .collect();
        // first term
        let mut t = vec![t0[0].clone()];
        // intermediate terms
        for (t0, t_shift) in t0.iter().skip(1).zip(t_shift.iter()) {
            t.push(self.add_calculation(Calculation::Add(t0.clone(), t_shift.clone())));
        }
        // final term
        t.push(t_shift.last().unwrap().clone());

        t
    }

    pub fn add_cross(&mut self, expr: &Expression) {
        assert!(expr.folding_degree() > 0);

        fn raise_and_combine(
            ev: &mut Evaluator,
            mut a: Vec<ValueSource>,
            mut b: Vec<ValueSource>,
        ) -> Vec<ValueSource> {
            let degree_a = a.len();
            let degree_b = b.len();
            let dif = degree_a.abs_diff(degree_b);

            if degree_a > degree_b {
                (0..dif).for_each(|_| b = ev.raise_by_u(b.clone()));
            } else {
                (0..dif).for_each(|_| a = ev.raise_by_u(a.clone()));
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
                    Variable::Seperator(index) => {
                        let cur =
                            ev.add_calculation(Calculation::Store(ValueSource::Seperator(*index)));
                        let run = ev.add_calculation(Calculation::Store(
                            ValueSource::RunningSeperator(*index),
                        ));
                        vec![cur, run]
                    }

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

        if self.targets().is_empty() {
            self.targets = targets_new;
            return;
        }
        self.targets = raise_and_combine(self, targets_new, self.targets.clone());

        // let dif = targets_new.len().abs_diff(self.targets.len());
        // let (t0, t1) = if targets_new.len() > self.targets.len() {
        //     let targets_old = self.raise_by_u(self.targets.clone(), dif);
        //     assert!(targets_new.len() == targets_old.len());
        //     (targets_old, targets_new)
        // } else {
        //     let targets_new = self.raise_by_u(targets_new, dif);
        //     let targets_old = self.targets.clone();
        //     assert!(targets_new.len() == targets_old.len());
        //     (targets_old, targets_new)
        // };

        // self.targets = t0
        //     .iter()
        //     .zip(t1.iter())
        //     .map(|(t0, t1)| self.add_calculation(Calculation::Add(t0.clone(), t1.clone())))
        //     .collect();
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
