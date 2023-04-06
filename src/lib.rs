use poly::{Polynomial, F};

pub mod calculation;
pub mod evaluation;
pub mod expression;
pub mod poly;
#[cfg(test)]
pub mod protocol;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rotation(pub i32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedQuery {
    pub index: usize,
    pub name: String,
    pub rotation: Rotation,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdviceQuery {
    pub index: usize,
    pub name: String,
    pub rotation: Rotation,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Challenge {
    pub index: usize,
    pub phase: u8,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum ValueSource {
    Intermediate(usize),
    PreviousValue(),
    // constants
    Fixed(usize, usize),
    Scalar(usize),
    // variables
    Advice(usize, usize),
    Challenge(usize),
    Y(usize),
    U(),
    // running variables
    RunningAdvice(usize, usize),
    RunningChallenge(usize),
    RunningU(),
    RunningY(usize),
}

impl Default for ValueSource {
    fn default() -> Self {
        ValueSource::Scalar(0)
    }
}

impl ValueSource {
    pub fn get(
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
        runnig_y: &[F],
        running_u: &F,
    ) -> F {
        match self {
            ValueSource::Intermediate(idx) => intermediates[*idx],
            ValueSource::PreviousValue() => *previous_value,
            // variables
            ValueSource::Advice(column_index, rotation) => {
                advice_values[*column_index][rotations[*rotation]]
            }
            ValueSource::Challenge(index) => challenges[*index],
            ValueSource::Y(index) => y[*index],
            ValueSource::U() => *u,
            // running variables
            ValueSource::RunningAdvice(column_index, rotation) => {
                running_advice_values[*column_index][rotations[*rotation]]
            }
            ValueSource::RunningChallenge(index) => running_challenges[*index],
            ValueSource::RunningY(index) => runnig_y[*index],
            ValueSource::RunningU() => *running_u,
            // constants
            ValueSource::Scalar(idx) => constants[*idx],
            ValueSource::Fixed(column_index, rotation) => {
                fixed_values[*column_index][rotations[*rotation]]
            }
        }
    }
}
