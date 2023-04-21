use poly::{Polynomial, F};

pub mod calculation;
pub mod evaluation;
pub mod expression;
pub mod poly;
#[cfg(test)]
pub mod protocol;

#[derive(Debug, Clone)]
pub struct Instance {
    values: Vec<Polynomial>,
    seperators: Vec<F>,
    u: F,
    error: Polynomial,
}

impl Instance {
    pub fn empty(size: usize) -> Self {
        Self {
            values: vec![],
            seperators: vec![],
            u: 1,
            error: Polynomial::empty(size),
        }
    }

    pub fn size(&self) -> usize {
        self.error.len()
    }

    pub fn fold(&mut self, current: Self, inter_polys: &[Polynomial], r: F) {
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

        running.values = running
            .values
            .iter()
            .zip(current.values.iter())
            .map(|(this, current)| this.fold(current, r))
            .collect();

        running.seperators = running
            .seperators
            .iter()
            .zip(current.seperators.iter())
            .map(|(this, current)| this + (current * r))
            .collect();

        running.u += current.u * r;
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum ValueSource {
    Intermediate(usize),
    // constants
    Fixed(usize),
    Scalar(usize),
    // variables
    Value(usize),
    U(),
    Seperator(usize),
    // running variables
    RunningValue(usize),
    RunningU(),
    RunningSeperator(usize),
}

impl Default for ValueSource {
    fn default() -> Self {
        ValueSource::Scalar(0)
    }
}

impl ValueSource {
    pub(crate) fn get(
        &self,
        // intermediaties
        row_index: usize,
        intermediates: &[F],
        // constants
        constants: &[F],
        fixed_values: &[Polynomial],

        current_instance: &Instance,
        running_instance: &Instance,
    ) -> F {
        match self {
            ValueSource::Intermediate(index) => intermediates[*index],
            // variables
            ValueSource::Value(index) => current_instance.values[*index][row_index],
            ValueSource::Seperator(index) => current_instance.seperators[*index],
            ValueSource::U() => current_instance.u,
            // running variables
            ValueSource::RunningValue(index) => running_instance.values[*index][row_index],
            ValueSource::RunningSeperator(index) => running_instance.seperators[*index],
            ValueSource::RunningU() => running_instance.u,
            // constants
            ValueSource::Scalar(index) => constants[*index],
            ValueSource::Fixed(column_index) => fixed_values[*column_index][row_index],
        }
    }
}
