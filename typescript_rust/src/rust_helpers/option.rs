use std::{borrow::Borrow, iter};

use gc::Gc;
use itertools::Either;

use crate::{Node, NodeArray, NodeInterface, Symbol, SymbolInterface, Type, TypeInterface};

pub trait NonEmpty {
    fn non_empty(self) -> Self;
    fn is_non_empty(self) -> bool;
}

impl NonEmpty for Option<&str> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl NonEmpty for Option<&String> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl NonEmpty for Option<String> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl<TItem> NonEmpty for Option<&[TItem]> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl<TItem> NonEmpty for Option<&Vec<TItem>> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl<TItem> NonEmpty for Option<Vec<TItem>> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl NonEmpty for Option<Gc<NodeArray>> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

pub trait GetOrInsertDefault {
    type Unwrapped;

    fn get_or_insert_default(&mut self) -> &mut Self::Unwrapped;
}

impl<TValue: Default> GetOrInsertDefault for Option<TValue> {
    type Unwrapped = TValue;

    fn get_or_insert_default(&mut self) -> &mut TValue {
        self.get_or_insert_with(|| Default::default())
    }
}

pub trait MapOrDefault {
    type Unwrapped;

    fn map_or_default<TMapped: Default>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> TMapped,
    ) -> TMapped;

    fn try_map_or_default<TMapped: Default, TError>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError>;
}

impl<TValue> MapOrDefault for Option<TValue> {
    type Unwrapped = TValue;

    fn map_or_default<TMapped: Default>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> TMapped,
    ) -> TMapped {
        self.map(mapper).unwrap_or_default()
    }

    fn try_map_or_default<TMapped: Default, TError>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError> {
        Ok(self.try_map(mapper)?.unwrap_or_default())
    }
}

pub trait BoolExt {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped>;
    fn try_then_and<TMapped, TError>(
        self,
        mapper: impl FnOnce() -> Result<Option<TMapped>, TError>,
    ) -> Result<Option<TMapped>, TError>;
    fn try_then<TMapped, TError>(
        self,
        mapper: impl FnOnce() -> Result<TMapped, TError>,
    ) -> Result<Option<TMapped>, TError>;
}

impl BoolExt for bool {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped> {
        self.then(mapper).flatten()
    }

    fn try_then_and<TMapped, TError>(
        self,
        mapper: impl FnOnce() -> Result<Option<TMapped>, TError>,
    ) -> Result<Option<TMapped>, TError> {
        Ok(if self { mapper()? } else { None })
    }

    fn try_then<TMapped, TError>(
        self,
        mapper: impl FnOnce() -> Result<TMapped, TError>,
    ) -> Result<Option<TMapped>, TError> {
        Ok(if self { Some(mapper()?) } else { None })
    }
}

pub trait NodeWrappered {
    fn node_wrappered(self) -> Option<Gc<Node>>;
}

impl<TValue: Borrow<Node>> NodeWrappered for Option<TValue> {
    fn node_wrappered(self) -> Option<Gc<Node>> {
        self.map(|node| node.borrow().node_wrapper())
    }
}

pub trait SymbolWrappered {
    fn symbol_wrappered(self) -> Option<Gc<Symbol>>;
}

impl<TValue: Borrow<Symbol>> SymbolWrappered for Option<TValue> {
    fn symbol_wrappered(self) -> Option<Gc<Symbol>> {
        self.map(|symbol| symbol.borrow().symbol_wrapper())
    }
}

pub trait TypeWrappered {
    fn type_wrappered(self) -> Option<Gc<Type>>;
}

impl<TValue: Borrow<Type>> TypeWrappered for Option<TValue> {
    fn type_wrappered(self) -> Option<Gc<Type>> {
        self.map(|type_| type_.borrow().type_wrapper())
    }
}

pub trait Matches {
    type Unwrapped;

    fn matches(self, predicate: impl FnOnce(Self::Unwrapped) -> bool) -> bool;
    fn try_matches<TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<bool, TError>,
    ) -> Result<bool, TError>;
}

impl<TValue> Matches for Option<TValue> {
    type Unwrapped = TValue;

    fn matches(self, predicate: impl FnOnce(Self::Unwrapped) -> bool) -> bool {
        self.map(predicate) == Some(true)
    }

    fn try_matches<TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<bool, TError>,
    ) -> Result<bool, TError> {
        match self {
            None => Ok(false),
            Some(value) => Ok(predicate(value)?),
        }
    }
}

pub trait UnwrapOrEmpty {
    type TIterator: Iterator;

    fn unwrap_or_empty(
        self,
    ) -> Either<Self::TIterator, iter::Empty<<Self::TIterator as Iterator>::Item>>;
}

impl<TIterator: Iterator> UnwrapOrEmpty for Option<TIterator> {
    type TIterator = TIterator;

    fn unwrap_or_empty(
        self,
    ) -> Either<TIterator, iter::Empty<<Self::TIterator as Iterator>::Item>> {
        self.map_or_else(
            || Either::Right(iter::empty()),
            |iterator| Either::Left(iterator),
        )
    }
}

pub trait OptionTry {
    type Unwrapped;

    fn try_get_or_insert_with<TError>(
        &mut self,
        predicate: impl FnOnce() -> Result<Self::Unwrapped, TError>,
    ) -> Result<&mut Self::Unwrapped, TError>;

    fn try_or_else<TError>(
        self,
        predicate: impl FnOnce() -> Result<Option<Self::Unwrapped>, TError>,
    ) -> Result<Option<Self::Unwrapped>, TError>;

    fn try_unwrap_or_else<TError>(
        self,
        predicate: impl FnOnce() -> Result<Self::Unwrapped, TError>,
    ) -> Result<Self::Unwrapped, TError>;

    fn try_and_then<TMapped, TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<Option<TMapped>, TError>,
    ) -> Result<Option<TMapped>, TError>;

    fn try_map<TMapped, TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<Option<TMapped>, TError>;

    fn try_filter<TError>(
        self,
        predicate: impl FnOnce(&Self::Unwrapped) -> Result<bool, TError>,
    ) -> Result<Option<Self::Unwrapped>, TError>;

    fn try_map_or_else<TMapped, TError>(
        self,
        default: impl FnOnce() -> Result<TMapped, TError>,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError>;

    fn try_map_or<TMapped, TError>(
        self,
        default: TMapped,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError>;
}

impl<TValue> OptionTry for Option<TValue> {
    type Unwrapped = TValue;

    fn try_get_or_insert_with<TError>(
        &mut self,
        predicate: impl FnOnce() -> Result<TValue, TError>,
    ) -> Result<&mut TValue, TError> {
        match self {
            Some(value) => Ok(value),
            None => {
                *self = Some(predicate()?);
                Ok(self.as_mut().unwrap())
            }
        }
    }

    fn try_or_else<TError>(
        self,
        predicate: impl FnOnce() -> Result<Option<Self::Unwrapped>, TError>,
    ) -> Result<Option<Self::Unwrapped>, TError> {
        match self {
            Some(value) => Ok(Some(value)),
            None => predicate(),
        }
    }

    fn try_unwrap_or_else<TError>(
        self,
        predicate: impl FnOnce() -> Result<Self::Unwrapped, TError>,
    ) -> Result<Self::Unwrapped, TError> {
        match self {
            Some(value) => Ok(value),
            None => predicate(),
        }
    }

    fn try_and_then<TMapped, TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<Option<TMapped>, TError>,
    ) -> Result<Option<TMapped>, TError> {
        match self {
            Some(value) => predicate(value),
            None => Ok(None),
        }
    }

    fn try_map<TMapped, TError>(
        self,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<Option<TMapped>, TError> {
        Ok(match self {
            Some(value) => Some(predicate(value)?),
            None => None,
        })
    }

    fn try_filter<TError>(
        self,
        predicate: impl FnOnce(&TValue) -> Result<bool, TError>,
    ) -> Result<Option<TValue>, TError> {
        Ok(match self {
            None => None,
            Some(value) => {
                if predicate(&value)? {
                    Some(value)
                } else {
                    None
                }
            }
        })
    }

    fn try_map_or_else<TMapped, TError>(
        self,
        default: impl FnOnce() -> Result<TMapped, TError>,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError> {
        match self {
            None => default(),
            Some(value) => predicate(value),
        }
    }

    fn try_map_or<TMapped, TError>(
        self,
        default: TMapped,
        predicate: impl FnOnce(Self::Unwrapped) -> Result<TMapped, TError>,
    ) -> Result<TMapped, TError> {
        match self {
            None => Ok(default),
            Some(value) => predicate(value),
        }
    }
}
