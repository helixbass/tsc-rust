use std::ops::Deref;

pub trait AsDoubleDeref {
    type Target: ?Sized;
    fn as_double_deref(&self) -> Option<&Self::Target>;
}

impl<T> AsDoubleDeref for Option<T>
where
    T: Deref,
    T::Target: Deref,
{
    type Target = <<T as Deref>::Target as Deref>::Target;

    fn as_double_deref(&self) -> Option<&Self::Target> {
        match self.as_ref() {
            Some(t) => Some(t.deref().deref()),
            None => None,
        }
    }
}
