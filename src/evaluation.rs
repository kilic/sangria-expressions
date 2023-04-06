use crate::{
    calculation::{Calculation, CalculationInfo},
    expression::{Constant, Expression, FlatExpression, Folding, FoldingExpression, Variable},
    poly::{Polynomial, F},
    Rotation, ValueSource,
};

#[derive(Default, Debug)]
pub struct EvaluationData {
    pub intermediates: Vec<F>,
    pub rotations: Vec<usize>,
}

impl MultiGraphEvaluator {
    pub fn from(gates: &Vec<Expression>) -> Self {
        assert!(!gates.is_empty());
        let inject_seperator = if gates.len() == 1 { false } else { true };

        let flat_expressions: Vec<FlatExpression> = gates.iter().map(|gate| gate.into()).collect();
        let max_variable_degree = flat_expressions
            .iter()
            .map(|expr| expr.max_variable_degree())
            .max()
            .unwrap();

        // vector of flattened expressions for each gate
        let folding_expression: Vec<FoldingExpression> = flat_expressions
            .iter()
            .enumerate()
            .map(|(gate_index, expr)| expr.cross(max_variable_degree, gate_index, inject_seperator))
            .collect();

        let number_of_levels = if inject_seperator {
            max_variable_degree + 2
        } else {
            max_variable_degree + 1
        };

        let mut evaluator = MultiGraphEvaluator::new(number_of_levels);

        for crossed_expression in folding_expression.iter() {
            let expressions = crossed_expression.into_expressions();

            for (level, expr) in expressions.iter().enumerate() {
                // println!("level: {}, expr: {:?}", level, expr.identifier());
                evaluator.add_expression(expr, level);
            }
        }

        evaluator
    }
}

#[derive(Clone, Debug)]
pub struct MultiGraphEvaluator {
    pub constants: Vec<F>,
    pub rotations: Vec<i32>,
    pub calculations: Vec<CalculationInfo>,
    pub level_map: Vec<Vec<usize>>,
    pub num_intermediates: usize,
}

impl MultiGraphEvaluator {
    pub fn new(number_of_levels: usize) -> Self {
        Self {
            constants: vec![0, 1, 2],
            rotations: Vec::new(),
            calculations: Vec::new(),
            level_map: vec![vec![]; number_of_levels],
            num_intermediates: 0,
        }
    }

    pub fn number_of_levels(&self) -> usize {
        self.level_map.len()
    }

    pub fn instance(&self) -> EvaluationData {
        EvaluationData {
            intermediates: vec![0; self.num_intermediates],
            rotations: vec![0usize; self.rotations.len()],
        }
    }

    fn add_rotation(&mut self, rotation: &Rotation) -> usize {
        let position = self.rotations.iter().position(|&c| c == rotation.0);
        match position {
            Some(pos) => pos,
            None => {
                self.rotations.push(rotation.0);
                self.rotations.len() - 1
            }
        }
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
                let idx = self.calculations.len();
                self.level_map[level].push(idx);
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
    pub fn add_expression(&mut self, expr: &Expression, level: usize) -> ValueSource {
        match expr {
            // variables
            Expression::Variable(folding) => match folding {
                Folding::Running(variable) => match variable {
                    Variable::U() => {
                        self.add_calculation(Calculation::Store(ValueSource::U()), level)
                    }
                    Variable::Y(idx) => {
                        self.add_calculation(Calculation::Store(ValueSource::Y(*idx)), level)
                    }
                    Variable::Challenge(challenge) => self.add_calculation(
                        Calculation::Store(ValueSource::Challenge(challenge.index)),
                        level,
                    ),
                    Variable::Advice(query) => {
                        let rot_idx = self.add_rotation(&query.rotation);
                        self.add_calculation(
                            Calculation::Store(ValueSource::Advice(query.index, rot_idx)),
                            level,
                        )
                    }
                },
                Folding::Current(variable) => match variable {
                    Variable::U() => {
                        self.add_calculation(Calculation::Store(ValueSource::RunningU()), level)
                    }
                    Variable::Y(idx) => {
                        self.add_calculation(Calculation::Store(ValueSource::RunningY(*idx)), level)
                    }
                    Variable::Challenge(challenge) => self.add_calculation(
                        Calculation::Store(ValueSource::RunningChallenge(challenge.index)),
                        level,
                    ),
                    Variable::Advice(query) => {
                        let rot_idx = self.add_rotation(&query.rotation);
                        self.add_calculation(
                            Calculation::Store(ValueSource::RunningAdvice(query.index, rot_idx)),
                            level,
                        )
                    }
                },
            },
            // constants
            Expression::Constant(constant) => match constant {
                Constant::Fixed(query) => {
                    let rot_idx = self.add_rotation(&query.rotation);
                    self.add_calculation(
                        Calculation::Store(ValueSource::Fixed(query.index, rot_idx)),
                        level,
                    )
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

    pub fn evaluate(
        &self,
        data: &mut EvaluationData,
        level: usize,
        idx: usize,
        rot_scale: i32,
        isize: i32,
        //
        previous_value: &F,
        fixed: &[Polynomial],
        //
        advice: &[Polynomial],
        challenges: &[F],
        y: &[F],
        u: &F,
        //
        running_advice: &[Polynomial],
        running_challenges: &[F],
        running_y: &[F],
        running_u: &F,
    ) -> F {
        fn get_rotation_idx(idx: usize, rot: i32, rot_scale: i32, isize: i32) -> usize {
            (((idx as i32) + (rot * rot_scale)).rem_euclid(isize)) as usize
        }

        for (rot_idx, rot) in self.rotations.iter().enumerate() {
            data.rotations[rot_idx] = get_rotation_idx(idx, *rot, rot_scale, isize);
        }

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
            data.intermediates[info.target] = info.calculation.evaluate(
                &data.rotations,
                &data.intermediates,
                previous_value,
                // cosntants
                &self.constants,
                fixed,
                //
                advice,
                challenges,
                y,
                u,
                //
                running_advice,
                running_challenges,
                running_y,
                running_u,
            );
        }

        if let Some(calc) = calculations.last() {
            data.intermediates[calc.target]
        } else {
            0
        }
    }
}
