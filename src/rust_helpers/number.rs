use serde::Serialize;
use std::fmt;
use std::hash;
use std::ops;

#[derive(Clone, Copy, Debug, PartialOrd, Serialize)]
pub struct Number(f64);

impl Number {
    pub fn new(value: f64) -> Self {
        if value.is_nan() {
            panic!("Tried to initialize Number with NaN: {}", value);
        }
        Self(value)
    }

    fn key(&self) -> u64 {
        self.0.to_bits()
    }

    pub fn value(&self) -> f64 {
        self.0
    }
}

impl hash::Hash for Number {
    fn hash<THasher>(&self, state: &mut THasher)
    where
        THasher: hash::Hasher,
    {
        self.key().hash(state)
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        self.key() == other.key()
    }
}

impl Eq for Number {}

// TODO: should really use TryFrom for this?
impl From<&str> for Number {
    fn from(str: &str) -> Self {
        Number::new(str.parse::<f64>().unwrap())
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl ops::Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.value())
    }
}
