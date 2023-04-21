use crate::{
    calculation::{Calculation, CalculationInfo},
    expression::{Constant, Expression, Folding, Variable},
    poly::{Polynomial, F},
    Instance, ValueSource,
};

impl Evaluator {
    // pub fn from(gates: &Vec<Expression>) -> Self {
    pub fn from(gates: impl Iterator<Item = Expression>) -> Self {
        let mut gates = gates.collect::<Vec<_>>();
        assert!(!gates.is_empty());

        gates.iter_mut().for_each(|gate| {
            let degree = gate.folding_degree();
            assert!(degree != 0);
            *gate = gate.relax();
            assert_eq!(gate.folding_degree(), degree);
        });

        let max_degree = gates
            .iter()
            .map(|gate| gate.folding_degree())
            .max()
            .unwrap();

        gates.iter_mut().for_each(|gate| {
            let degree = gate.folding_degree();
            let dif = max_degree - degree;
            if dif != 0 {
                let u: Expression = Variable::U().into();
                let u_power = u.pow(dif);
                *gate = gate.clone() * u_power;
                assert_eq!(gate.folding_degree(), max_degree);
            }
        });

        let number_of_levels = max_degree + 2; //  one for multiplication and one for seperator
        let crossed_gates: Vec<Vec<Expression>> = gates
            .iter()
            .enumerate()
            .map(|(index, gate)| {
                let crossed_gate = gate.cross_with_seperator(index);
                assert_eq!(crossed_gate.len(), number_of_levels);
                crossed_gate
            })
            .collect();

        let mut evaluator = Evaluator::new(number_of_levels);
        for level in 0..number_of_levels {
            let gate_in_level = crossed_gates
                .iter()
                .map(|crossed_gate| crossed_gate[level].clone())
                .collect::<Vec<_>>();
            let level_sum = Expression::sum(&gate_in_level);
            evaluator.add_expression(&level_sum, level);
        }

        evaluator
    }
}

#[derive(Clone, Debug)]
pub struct Evaluator {
    pub(crate) constants: Vec<F>,
    pub(crate) calculations: Vec<CalculationInfo>,
    pub(crate) level_map: Vec<Vec<usize>>,
    pub(crate) num_intermediates: usize,
}

impl Evaluator {
    fn new(number_of_levels: usize) -> Self {
        Self {
            constants: vec![0, 1, 2],
            calculations: Vec::new(),
            level_map: vec![vec![]; number_of_levels],
            num_intermediates: 0,
        }
    }

    pub fn number_of_levels(&self) -> usize {
        self.level_map.len()
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

    fn add_calculation(&mut self, calculation: Calculation, level: usize) -> ValueSource {
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
                let index = self.calculations.len();
                self.level_map[level].push(index);
                self.calculations.push(CalculationInfo {
                    calculation,
                    target,
                });
                self.num_intermediates += 1;
                ValueSource::Intermediate(target)
            }
        }
    }

    /// Generates an optimized evaluation for the expression
    pub(crate) fn add_expression(&mut self, expr: &Expression, level: usize) -> ValueSource {
        match expr {
            // variables
            Expression::Variable(folding) => match folding {
                Folding::Current(variable) => match variable {
                    Variable::U() => {
                        self.add_calculation(Calculation::Store(ValueSource::U()), level)
                    }
                    Variable::Seperator(index) => self
                        .add_calculation(Calculation::Store(ValueSource::Seperator(*index)), level),

                    Variable::Value(index) => {
                        self.add_calculation(Calculation::Store(ValueSource::Value(*index)), level)
                    }
                },
                Folding::Running(variable) => match variable {
                    Variable::U() => {
                        self.add_calculation(Calculation::Store(ValueSource::RunningU()), level)
                    }
                    Variable::Seperator(index) => self.add_calculation(
                        Calculation::Store(ValueSource::RunningSeperator(*index)),
                        level,
                    ),

                    Variable::Value(index) => self.add_calculation(
                        Calculation::Store(ValueSource::RunningValue(*index)),
                        level,
                    ),
                },
            },
            // constants
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => {
                    self.add_calculation(Calculation::Store(ValueSource::Fixed(*index)), level)
                }
                Constant::Scalar(e) => self.add_constant(e),
            },
            // relations
            Expression::Negated(a) => match &**a {
                Expression::Constant(constant) => match constant {
                    Constant::Scalar(scalar) => self.add_constant(&-scalar),
                    _ => {
                        let result_a = self.add_expression(a, level);
                        match result_a {
                            ValueSource::Scalar(0) => result_a,
                            _ => self.add_calculation(Calculation::Negate(result_a), level),
                        }
                    }
                },
                _ => {
                    let result_a = self.add_expression(a, level);
                    match result_a {
                        ValueSource::Scalar(0) => result_a,
                        _ => self.add_calculation(Calculation::Negate(result_a), level),
                    }
                }
            },
            Expression::Sum(a, b) => {
                // Undo subtraction stored as a + (-b) in expressions
                match &**b {
                    Expression::Negated(b_int) => {
                        let result_a = self.add_expression(a, level);
                        let result_b = self.add_expression(b_int, level);
                        if result_a == ValueSource::Scalar(0) {
                            self.add_calculation(Calculation::Negate(result_b), level)
                        } else if result_b == ValueSource::Scalar(0) {
                            result_a
                        } else {
                            self.add_calculation(Calculation::Sub(result_a, result_b), level)
                        }
                    }
                    _ => {
                        let result_a = self.add_expression(a, level);
                        let result_b = self.add_expression(b, level);
                        if result_a == ValueSource::Scalar(0) {
                            result_b
                        } else if result_b == ValueSource::Scalar(0) {
                            result_a
                        } else if result_a <= result_b {
                            self.add_calculation(Calculation::Add(result_a, result_b), level)
                        } else {
                            self.add_calculation(Calculation::Add(result_b, result_a), level)
                        }
                    }
                }
            }
            Expression::Product(a, b) => {
                let result_a = self.add_expression(a, level);
                let result_b = self.add_expression(b, level);
                if result_a == ValueSource::Scalar(0) || result_b == ValueSource::Scalar(0) {
                    ValueSource::Scalar(0)
                } else if result_a == ValueSource::Scalar(1) {
                    result_b
                } else if result_b == ValueSource::Scalar(1) {
                    result_a
                } else if result_a == ValueSource::Scalar(2) {
                    self.add_calculation(Calculation::Double(result_b), level)
                } else if result_b == ValueSource::Scalar(2) {
                    self.add_calculation(Calculation::Double(result_a), level)
                } else if result_a == result_b {
                    self.add_calculation(Calculation::Square(result_a), level)
                } else if result_a <= result_b {
                    self.add_calculation(Calculation::Mul(result_a, result_b), level)
                } else {
                    self.add_calculation(Calculation::Mul(result_b, result_a), level)
                }
            }
            Expression::Scaled(a, f) => {
                if *f == 0 {
                    ValueSource::Scalar(0)
                } else if *f == 1 {
                    self.add_expression(a, level)
                } else {
                    let cst = self.add_constant(f);
                    let result_a = self.add_expression(a, level);
                    self.add_calculation(Calculation::Mul(result_a, cst), level)
                }
            }
        }
    }

    pub fn eval(
        &self,
        intermediates: &mut [F],
        level: usize,
        row_index: usize,
        fixed: &[Polynomial],
        //
        current_instance: &Instance,
        running_instance: &Instance,
    ) -> F {
        let calculation_indexes = &self.level_map[level];
        let calculations = self
            .calculations
            .iter()
            .enumerate()
            .filter_map(|(i, e)| {
                if calculation_indexes.contains(&i) {
                    Some(e.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for info in calculations.iter() {
            intermediates[info.target] = info.calculation.eval(
                row_index,
                intermediates,
                &self.constants,
                fixed,
                current_instance,
                running_instance,
            );
        }

        if let Some(calc) = calculations.last() {
            intermediates[calc.target]
        } else {
            0
        }
    }
}
