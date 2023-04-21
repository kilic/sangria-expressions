use crate::{
    poly::{Polynomial, F},
    Instance, ValueSource,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Calculation {
    Add(ValueSource, ValueSource),
    Sub(ValueSource, ValueSource),
    Mul(ValueSource, ValueSource),
    Square(ValueSource),
    Double(ValueSource),
    Negate(ValueSource),
    Store(ValueSource),
}

#[derive(Clone, Debug)]
pub(crate) struct CalculationInfo {
    pub(crate) calculation: Calculation,
    pub(crate) target: usize,
}

impl Calculation {
    pub(crate) fn eval(
        &self,
        // intermediaties
        row_index: usize,
        intermediates: &[F],
        previous_value: &F, // TODO: this can be any value maybe better call it as 'aux'
        // constants
        constants: &[F],
        fixed_values: &[Polynomial],

        current_instance: &Instance,
        running_instance: &Instance,
    ) -> F {
        let get_value = |value: &ValueSource| {
            value.get(
                row_index,
                intermediates,
                previous_value,
                constants,
                fixed_values,
                current_instance,
                running_instance,
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
