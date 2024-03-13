use std::{fmt, hash, ops};

use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialOrd, Serialize)]
pub struct Number(f64);

impl Number {
    pub fn new(value: f64) -> Self {
        Self(value)
    }

    fn key(&self) -> u64 {
        self.0.to_bits()
    }

    pub fn value(&self) -> f64 {
        self.0
    }

    pub fn integer_value(&self) -> i64 {
        // TODO: should check that we're an "integer"?
        self.value() as i64
    }

    // per https://github.com/boa-dev/boa/blob/main/core/engine/src/builtins/number/mod.rs
    pub fn as_string(&self) -> String {
        let radix = 10;

        if radix == 10 {
            return self.to_js_string();
        }
        unimplemented!()
    }

    pub fn to_js_string(&self) -> String {
        let mut buffer = ryu_js::Buffer::new();
        buffer.format(self.0).to_owned()
    }

    pub fn is_finite(&self) -> bool {
        self.0.is_finite()
    }

    pub fn is_nan(&self) -> bool {
        self.0.is_nan()
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
        self.as_string().fmt(f)
    }
}

impl ops::Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.value())
    }
}

impl ops::Not for Number {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::new((!self.integer_value()) as f64)
    }
}

impl ops::BitOr for Number {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::new((self.integer_value() | rhs.integer_value()) as f64)
    }
}

impl ops::BitAnd for Number {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::new((self.integer_value() & rhs.integer_value()) as f64)
    }
}

impl ops::Shr for Number {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Self::new((self.integer_value() >> rhs.integer_value()) as f64)
    }
}

impl ops::Shl for Number {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        Self::new((self.integer_value() << rhs.integer_value()) as f64)
    }
}

impl ops::BitXor for Number {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::new((self.integer_value() ^ rhs.integer_value()) as f64)
    }
}

impl ops::Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.value() * rhs.value())
    }
}

impl ops::Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.value() / rhs.value())
    }
}

impl ops::Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.value() + rhs.value())
    }
}

impl ops::Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.value() - rhs.value())
    }
}

impl ops::Rem for Number {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::new(self.value() % rhs.value())
    }
}
