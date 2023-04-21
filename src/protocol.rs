use crate::{
    evaluation::Evaluator,
    expression::{Constant, Expression, Folding, Variable},
    poly::Polynomial,
    Instance,
};
use rand::Rng;
use rand_core::OsRng;

use super::F;

fn rand_scalar() -> F {
    let rng = &mut OsRng;
    rng.gen()
}

fn rand_values(n: usize) -> Vec<F> {
    (0..n).map(|_| rand_scalar()).collect::<Vec<_>>()
}

fn rand_polys(n: usize, size: usize) -> Vec<Polynomial> {
    (0..n).map(|_| Polynomial::rand(size)).collect::<Vec<_>>()
}

fn value(index: usize) -> Expression {
    Variable::Value(index).into()
}

// fn fixed(index: usize) -> Expression {
//     Constant::Fixed(index).into()
// }

impl Expression {
    pub(crate) fn number_of_values(&self) -> usize {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(var) => match var {
                    Variable::Value(index) => *index + 1,
                    Variable::U() => 0,
                    Variable::Seperator(_) => unreachable!(),
                },
                Folding::Running(_) => unreachable!(),
            },
            Expression::Constant(_) => 0,
            Expression::Negated(a) => a.number_of_values(),
            Expression::Sum(a, b) => std::cmp::max(a.number_of_values(), b.number_of_values()),
            Expression::Product(a, b) => std::cmp::max(a.number_of_values(), b.number_of_values()),
            Expression::Scaled(a, _) => a.number_of_values(),
        }
    }

    pub(crate) fn number_of_fixed(&self) -> usize {
        match self {
            Expression::Variable(folding) => match folding {
                Folding::Current(_) => 0,
                Folding::Running(_) => unreachable!(),
            },
            Expression::Constant(constant) => match constant {
                Constant::Fixed(index) => *index + 1,
                _ => 0,
            },
            Expression::Negated(a) => a.number_of_fixed(),
            Expression::Sum(a, b) => std::cmp::max(a.number_of_fixed(), b.number_of_fixed()),
            Expression::Product(a, b) => std::cmp::max(a.number_of_fixed(), b.number_of_fixed()),
            Expression::Scaled(a, _) => a.number_of_fixed(),
        }
    }

    fn randomish(number_of_triples: usize) -> Self {
        let mut var_index = 0;
        let mut fix_index = 0;

        let mut pick_value = || -> Expression {
            let mut rng = OsRng;

            let pick_var = rng.gen_bool(0.5);
            let existing = rng.gen_bool(0.5);
            let expr: Expression = if pick_var || var_index == 0 {
                if existing && var_index != 0 {
                    let index: usize = rng.gen_range(0..var_index);
                    Variable::Value(index).into()
                } else {
                    var_index += 1;
                    Variable::Value(var_index - 1).into()
                }
            } else {
                if existing && fix_index != 0 {
                    let index: usize = rng.gen_range(0..fix_index);
                    Constant::Fixed(index).into()
                } else {
                    fix_index += 1;
                    Constant::Fixed(fix_index - 1).into()
                }
            };
            expr
        };
        let mut make_triple = || -> Expression {
            let mut rng = OsRng;
            let a = pick_value();
            let b = pick_value();
            let c = pick_value();
            match rng.gen_range(0..9) {
                0 => a * b * c,
                1 => a * b + c,
                2 => a * b - c,
                3 => a + b * c,
                4 => a + b + c,
                5 => a + b - c,
                6 => a - b * c,
                7 => a - b + c,
                8 => a - b - c,
                _ => unreachable!(),
            }
        };

        let relate = |expressions: &[Expression]| -> Expression {
            let mut rng = OsRng;
            let mut expr = expressions[0].clone();
            for i in 1..expressions.len() {
                let expr2 = expressions[i].clone();
                expr = match rng.gen_range(0..3) {
                    0 => expr * expr2,
                    1 => expr + expr2,
                    2 => expr - expr2,
                    _ => unreachable!(),
                }
            }
            expr
        };

        let triples = (0..number_of_triples)
            .map(|_| make_triple())
            .collect::<Vec<_>>();
        relate(&triples[..])
    }
}

#[test]
fn test_u_injection() {
    let a0 = value(0);
    let a1 = value(1);
    let a2 = value(2);

    let size = 4;
    let number_of_expressions = 1;

    let (values, fixed) = (rand_polys(100, size), rand_polys(100, size));
    let seperators = rand_values(number_of_expressions);
    let instance = Instance {
        values,
        seperators,
        u: 1,
        error: Polynomial::empty(size),
    };
    let expr0 = (a0.clone() * a1.clone() + a2.clone())
        * (a0.clone() * a1.clone() * a2.clone() + a0.clone() * a1.clone() + a0.clone());

    let relax_expr0 = expr0.relax();
    let e0 = expr0.eval_poly(&fixed[..], &instance, &Instance::empty(size));
    let e1 = relax_expr0.eval_poly(&fixed[..], &instance, &Instance::empty(size));
    assert_eq!(e0, e1);

    let expr1 = (a0.clone() * a1.clone() * a2.clone() + a0.clone() * a1.clone() + a0.clone())
        * (a0 * a1 + a2);
    let relax_expr1 = expr1.relax();
    let e0 = expr1.eval_poly(&fixed[..], &instance, &Instance::empty(size));
    assert_eq!(e0, e1);
    let e1 = relax_expr1.eval_poly(&fixed[..], &instance, &Instance::empty(size));
    assert_eq!(e0, e1);

    for _ in 0..1000 {
        let expr = Expression::randomish(10);
        let d0 = expr.folding_degree();
        let relax_expr = expr.relax();
        let d1 = relax_expr.folding_degree();
        assert!(d0 == d1);
        let e0 = expr0.eval_poly(&fixed[..], &instance, &Instance::empty(size));
        let e1 = relax_expr0.eval_poly(&fixed[..], &instance, &Instance::empty(size));
        assert_eq!(e0, e1);
    }
}

fn binom_coeffs(degree: usize) -> Vec<i32> {
    let mut u = vec![1];
    for i in 1..degree {
        let mut v = vec![0; i + 1];
        v[0] = 1;
        v[i] = 1;
        for j in 1..i {
            v[j] = u[j - 1] + u[j];
        }
        u = v;
    }
    u
}

#[test]
fn test_cross_terms() {
    let size = 4;
    let number_of_expressions = 1;
    let (values, fixed) = (rand_polys(50, size), rand_polys(50, size));
    let seperators = rand_values(number_of_expressions);
    let instance = Instance {
        values,
        seperators,
        u: 1,
        error: Polynomial::empty(size),
    };

    let a0: Expression = Variable::Value(0).into();
    let a1: Expression = Variable::Value(1).into();
    let a2: Expression = Variable::Value(2).into();
    let expr = a0 * a1 + a2;
    let relax_expr = expr.relax();
    let degree = relax_expr.folding_degree();
    let cross_terms = relax_expr.cross_with_seperator(0);
    assert_eq!(
        cross_terms.len(),
        degree + 2 /* one for multiplication and one for seperator */
    );

    let last = cross_terms.last().unwrap();
    let first = cross_terms.first().unwrap();

    let e0 = first.eval_poly(&fixed[..], &instance, &Instance::empty(size));
    let e1 = last.eval_poly(&fixed[..], &Instance::empty(size), &instance);
    assert_eq!(e0, e1);

    let factors = binom_coeffs(cross_terms.len());
    for (term, factor) in cross_terms.iter().zip(factors.iter()) {
        let e1 = term.eval_poly(&fixed[..], &instance, &instance);
        assert_eq!(e0.clone() * *factor, e1);
    }

    // a bit fuz under random expresions
    for _ in 0..1000 {
        let expr = Expression::randomish(10);
        let relax_expr = expr.relax();
        let degree = relax_expr.folding_degree();
        let cross_terms = relax_expr.cross_with_seperator(0);
        assert_eq!(
            cross_terms.len(),
            degree + 2 /* one for multiplication and one for seperator */
        );

        let last = cross_terms.last().unwrap();
        let first = cross_terms.first().unwrap();

        let e0 = first.eval_poly(&fixed[..], &instance, &Instance::empty(size));
        let e1 = last.eval_poly(&fixed[..], &Instance::empty(size), &instance);
        assert_eq!(e0, e1);

        let factors = binom_coeffs(cross_terms.len());
        for (term, factor) in cross_terms.iter().zip(factors.iter()) {
            let e1 = term.eval_poly(&fixed[..], &instance, &instance);
            assert_eq!(e0.clone() * *factor, e1);
        }
    }
}

fn check(instance: &Instance, fixed: &[Polynomial], evaluator: &Evaluator) {
    let data = &mut evaluator.intermediates();
    let size = instance.error.len();
    let mut error = Polynomial::empty(size);
    for index in 0..size {
        let value = evaluator.eval(data, 0, index, &0, fixed, instance, &Instance::empty(size));
        error.0[index] = value;
    }
    assert_eq!(instance.error, error);
}

#[test]
fn test_sangria() {
    struct Gate {
        expr: Expression,
        result_index: usize,
    }

    impl Gate {
        fn random(number_of_triples: usize) -> Self {
            let expr = Expression::randomish(number_of_triples);
            Gate {
                expr,
                result_index: 0,
            }
        }

        fn number_of_values(&self) -> usize {
            self.expr.number_of_values()
        }

        fn number_of_fixed(&self) -> usize {
            self.expr.number_of_fixed()
        }

        fn satisfied(&self) -> Expression {
            self.expr.clone() - value(self.result_index)
        }

        fn set_result(&self, fixed: &[Polynomial], values: &mut [Polynomial]) {
            let size = values[0].0.len();
            let test_instance = Instance {
                values: values.to_vec(),
                seperators: vec![],
                u: 1,
                error: Polynomial::empty(size),
            };
            values[self.result_index] =
                self.expr
                    .eval_poly(fixed, &test_instance, &Instance::empty(size));
            let test_instance = Instance {
                values: values.to_vec(),
                seperators: vec![],
                u: 1,
                error: Polynomial::empty(size),
            };
            assert!(self
                .satisfied()
                .eval_poly(fixed, &test_instance, &Instance::empty(size))
                .is_zero());
        }
    }

    {
        let size = 4;
        let number_of_gates = 10;
        let number_of_triples = 10;

        let mut gates = (0..number_of_gates)
            .map(|_| Gate::random(number_of_triples))
            .collect::<Vec<_>>();

        let mut number_of_values = gates
            .iter()
            .map(|gate| gate.number_of_values())
            .max()
            .unwrap();

        let number_of_fixed = gates
            .iter()
            .map(|gate| gate.number_of_fixed())
            .max()
            .unwrap_or_default();

        let fixed_polys = rand_polys(number_of_fixed, size);

        for gate in gates.iter_mut() {
            gate.result_index = number_of_values;
            number_of_values += 1;
        }

        let evaluator = Evaluator::from(gates.iter().map(|gate| gate.satisfied()));

        let mut empty_values = vec![Polynomial::empty(size); number_of_values];
        for gate in gates.iter() {
            gate.set_result(&fixed_polys, &mut empty_values);
        }
        let seperators = (0..number_of_gates)
            .map(|_| rand_scalar()) // could be zero too
            .collect::<Vec<_>>();
        let mut running_instance = Instance {
            values: empty_values,
            seperators,
            u: 1,
            error: Polynomial::empty(size),
        };
        check(&running_instance, &fixed_polys[..], &evaluator);

        for i in 0..100 {
            let intermediates = &mut evaluator.intermediates();
            let number_of_levels = evaluator.number_of_levels();
            let mut random_values = rand_polys(number_of_values, size);
            for gate in gates.iter() {
                gate.set_result(&fixed_polys, &mut random_values);
            }

            let seperators = (0..number_of_gates)
                .map(|_| rand_scalar())
                .collect::<Vec<_>>();
            let current_instance = Instance {
                values: random_values,
                seperators,
                u: 1,
                error: Polynomial::empty(size),
            };
            check(&current_instance, &fixed_polys[..], &evaluator);

            let mut cross_polys = vec![Polynomial::empty(size); number_of_levels];
            for row_index in 0..size {
                for (level, cross_poly) in cross_polys.iter_mut().enumerate() {
                    cross_poly.0[row_index] = evaluator.eval(
                        intermediates,
                        level,
                        row_index,
                        &0,
                        &fixed_polys,
                        &current_instance,
                        &running_instance,
                    );
                }
            }

            // current instance goes to 0th index and should satisfy the constraint as being zero
            assert!(cross_polys.first().unwrap().is_zero());
            if i == 0 {
                // running instance goes to the zero only in the first round
                assert!(cross_polys.last().unwrap().is_zero());
            }

            // trim the satisfied constraints
            let inter_polys = &cross_polys[1..cross_polys.len() - 1];
            // draw folding challenge
            let r = rand_scalar();
            // fold in to running instance
            running_instance.fold(current_instance, inter_polys, r);
            // verify folded instance
            check(&running_instance, &fixed_polys, &evaluator)
        }
    }
}
