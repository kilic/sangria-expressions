use crate::{
    poly::{Polynomial, F},
    ValueSource,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Calculation {
    Add(ValueSource, ValueSource),
    Sub(ValueSource, ValueSource),
    Mul(ValueSource, ValueSource),
    Square(ValueSource),
    Double(ValueSource),
    Negate(ValueSource),
    Store(ValueSource),
}

#[derive(Clone, Debug)]
pub struct CalculationInfo {
    pub calculation: Calculation,
    pub target: usize,
}

impl Calculation {
    pub fn evaluate(
        &self,
        // intermediaties
        rotations: &[usize],
        intermediates: &[F],
        previous_value: &F, // TODO: this can be any value maybe better call it as 'aux'
        // constants
        constants: &[F],
        fixed_values: &[Polynomial],
        // variables
        advice_values: &[Polynomial],
        challenges: &[F],
        y: &[F],
        u: &F,
        // running variables
        running_advice_values: &[Polynomial],
        running_challenges: &[F],
        running_y: &[F],
        running_u: &F,
    ) -> F {
        let get_value = |value: &ValueSource| {
            value.get(
                rotations,
                intermediates,
                previous_value,
                constants,
                fixed_values,
                advice_values,
                challenges,
                y,
                u,
                running_advice_values,
                running_challenges,
                running_y,
                running_u,
            )
        };
        match self {
            Calculation::Add(a, b) => get_value(a) + get_value(b),
            Calculation::Sub(a, b) => get_value(a) - get_value(b),
            Calculation::Mul(a, b) => get_value(a) * get_value(b),
            Calculation::Square(v) => get_value(v) * get_value(v),
            Calculation::Double(v) => 2 * get_value(v),
            Calculation::Negate(v) => -get_value(v),
            Calculation::Store(v) => get_value(v),
        }
    }
}
