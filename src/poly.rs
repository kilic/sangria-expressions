use std::ops::{Add, Index, Mul};

pub type F = i32;

impl From<Vec<F>> for Polynomial {
    fn from(v: Vec<F>) -> Self {
        Self(v)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Polynomial(pub Vec<F>);

impl Index<usize> for Polynomial {
    type Output = F;

    fn index(&self, index: usize) -> &F {
        self.0.index(index)
    }
}

impl Polynomial {
    pub fn is_zero(&self) -> bool {
        for i in 0..self.0.len() {
            if self.0[i] != 0 {
                return false;
            }
        }
        true
    }

    #[cfg(test)]
    pub fn rand(size: usize) -> Self {
        use rand::Rng;
        use rand_core::OsRng;
        let rng = &mut OsRng;
        let mut coeffs = vec![0; size];
        for i in 0..size {
            coeffs[i] = rng.gen();
        }
        Self(coeffs)
    }

    pub fn empty(size: usize) -> Self {
        let coeffs = vec![0; size];
        Self(coeffs)
    }

    pub fn fold(&self, other: &Polynomial, r: F) -> Self {
        Self(
            self.0
                .iter()
                .zip(other.0.iter())
                .map(|(this, other)| this + r * other)
                .collect::<Vec<_>>(),
        )
    }
}

impl Add for Polynomial {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        self.0
            .iter()
            .zip(rhs.0.iter())
            .map(|(a, b)| a + b)
            .collect::<Vec<_>>()
            .into()
    }
}

impl Mul<F> for Polynomial {
    type Output = Self;
    fn mul(self, rhs: F) -> Self::Output {
        self.0.iter().map(|a| a * rhs).collect::<Vec<_>>().into()
    }
}

impl Add for &Polynomial {
    type Output = Polynomial;
    fn add(self, rhs: Self) -> Self::Output {
        self.0
            .iter()
            .zip(rhs.0.iter())
            .map(|(a, b)| a + b)
            .collect::<Vec<_>>()
            .into()
    }
}

impl Mul<F> for &Polynomial {
    type Output = Polynomial;
    fn mul(self, rhs: F) -> Self::Output {
        self.0.iter().map(|a| a * rhs).collect::<Vec<_>>().into()
    }
}
