use std::hash;

#[derive(Clone, Copy)]
pub struct Number(f64);

impl Number {
    fn new(value: f64) -> Self {
        if value.is_nan() {
            panic!("Tried to initialize Number with NaN: {}", value);
        }
        Self(value)
    }

    fn key(&self) -> u64 {
        self.0.to_bits()
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

impl From<&str> for Number {
    fn from(str: &str) -> Self {
        Number::new(str.parse::<f64>().unwrap())
    }
}
