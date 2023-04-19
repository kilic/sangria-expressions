use crate::{
    evaluation::MultiGraphEvaluator,
    expression::Expression,
    poly::{Polynomial, F},
};

use crate::{AdviceQuery, FixedQuery};
use rand::Rng;
use rand_core::OsRng;

fn rand_scalar() -> F {
    let rng = &mut OsRng;
    let u: F = rng.gen();
    u
}

fn advice(name: &str, index: usize, rotation: i32) -> Expression {
    Expression::advice(AdviceQuery {
        name: name.to_string(),
        index,
        rotation: crate::Rotation(rotation),
    })
}

fn fixed(name: &str, index: usize, rotation: i32) -> Expression {
    Expression::fixed(FixedQuery {
        name: name.to_string(),
        index,
        rotation: crate::Rotation(rotation),
    })
}

#[derive(Debug, Clone)]
struct Instance {
    advice: Vec<Polynomial>,
    challenges: Vec<F>,
    y: Vec<F>,
    error: Polynomial,
    u: F,
}

impl Instance {
    fn fold(&mut self, current: Self, inter_polys: &[Polynomial], r: F) {
        let running = self;
        assert!(current.error.is_zero());
        assert!(current.u == 1);

        running.error = running.error.clone()
            + inter_polys
                .iter()
                .skip(1)
                .fold(inter_polys[0].clone() * r, |acc, t_i| {
                    (acc + t_i.clone()) * r
                });

        running.advice = running
            .advice
            .iter()
            .zip(current.advice.iter())
            .map(|(this, current)| this.fold(current, r))
            .collect();
        running.challenges = running
            .challenges
            .iter()
            .zip(current.challenges.iter())
            .map(|(this, current)| this + (current * r))
            .collect();
        running.y = running
            .y
            .iter()
            .zip(current.y.iter())
            .map(|(this, current)| this + (current * r))
            .collect();

        running.u = running.u + (current.u * r);
    }
}

fn check(instance: &Instance, fixed: &[Polynomial], evaluator: &MultiGraphEvaluator) {
    let data = &mut evaluator.instance();
    let size = instance.advice[0].0.len();
    let mut error = Polynomial::empty(size);
    for idx in 0..size {
        let value = evaluator.evaluate(
            data,
            0,
            idx,
            1,
            size as i32,
            &0,
            fixed,
            //
            &instance.advice,
            &instance.challenges,
            &instance.y,
            &instance.u,
            //
            &[],
            &[],
            &[],
            &1,
        );
        error.0[idx] = value;
    }
    assert_eq!(instance.error, error);
}

#[test]
fn test_sangria() {
    use rand::Rng;
    use rand_core::OsRng;

    let size = 4;

    let rand_advices_0 = |size: usize, q_poly: Polynomial| -> Vec<Polynomial> {
        let a_poly = Polynomial::rand(size);
        let b_poly = Polynomial::rand(size);
        let c_poly = a_poly
            .0
            .iter()
            .zip(b_poly.0.iter())
            .zip(q_poly.0.iter())
            .map(|((a, b), q)| {
                if *q == 1 {
                    a * b
                } else {
                    let rng = &mut OsRng;
                    rng.gen()
                }
            })
            .collect::<Vec<_>>();
        let c_poly = Polynomial(c_poly);
        vec![a_poly, b_poly, c_poly]
    };

    let rand_advices_1 = |size: usize, q_poly: Polynomial| -> Vec<Polynomial> {
        let a_poly = Polynomial::rand(size);
        let b_poly = Polynomial::rand(size);
        let c_poly = a_poly
            .0
            .iter()
            .zip(b_poly.0.iter())
            .zip(q_poly.0.iter())
            .map(|((a, b), q)| {
                if *q == 1 {
                    a + b
                } else {
                    let rng = &mut OsRng;
                    rng.gen()
                }
            })
            .collect::<Vec<_>>();
        let c_poly = Polynomial(c_poly);
        vec![a_poly, b_poly, c_poly]
    };

    let a = advice("a", 0, 0);
    let b = advice("b", 1, 0);
    let c = advice("c", 2, 0);
    let q0 = fixed("q0", 0, 0);
    let gate_0 = (a.clone() * b.clone() - c.clone()) * q0.clone();
    let q_poly_0 = Polynomial(vec![1, 1, 1, 1]);

    let a = advice("a", 3, 0);
    let b = advice("b", 4, 0);
    let c = advice("c", 5, 0);
    let q0 = fixed("q0", 1, 0);
    let gate_1 = (a.clone() + b.clone() - c.clone()) * q0.clone();
    let q_poly_1 = Polynomial(vec![1, 1, 1, 1]);

    let evaluator = MultiGraphEvaluator::from(&vec![gate_0.clone(), gate_1.clone()]);

    let mut running_instance = Instance {
        advice: vec![Polynomial::empty(size); 6],
        challenges: vec![],
        y: vec![rand_scalar(), rand_scalar()],
        error: Polynomial::empty(size),
        u: 1,
    };
    let fixed = &[q_poly_0.clone(), q_poly_1.clone()];
    // empty instance should pass
    check(&running_instance, fixed, &evaluator);

    for i in 0..1000 {
        let data = &mut evaluator.instance();
        let number_of_levels = evaluator.number_of_levels();

        let advice_0 = rand_advices_0(size, q_poly_0.clone());
        let advice_1 = rand_advices_1(size, q_poly_1.clone());

        let advice = advice_0
            .iter()
            .chain(advice_1.iter())
            .cloned()
            .collect::<Vec<_>>();
        let current_instance = Instance {
            advice,
            challenges: vec![],
            y: vec![rand_scalar(), rand_scalar()],
            error: Polynomial::empty(size),
            u: 1,
        };
        // verify current instance
        check(&current_instance, fixed, &evaluator);

        let mut cross_polys = vec![Polynomial::empty(size); number_of_levels];

        for idx in 0..size {
            for level in 0..number_of_levels {
                let value = evaluator.evaluate(
                    data,
                    level,
                    idx,
                    1,
                    size as i32,
                    &0,
                    fixed,
                    //
                    &current_instance.advice,
                    &current_instance.challenges,
                    &current_instance.y,
                    &current_instance.u,
                    //
                    &running_instance.advice,
                    &running_instance.challenges,
                    &running_instance.y,
                    &running_instance.u,
                );
                cross_polys[level].0[idx] = value;
            }
        }

        assert!(cross_polys.first().unwrap().is_zero());
        if i == 0 {
            assert!(cross_polys.last().unwrap().is_zero());
        }

        let inter_polys = &cross_polys[1..cross_polys.len() - 1];
        let r = rand_scalar();
        running_instance.fold(current_instance, inter_polys, r);
        // verify folded instance
        check(&running_instance, fixed, &evaluator)
    }
}
