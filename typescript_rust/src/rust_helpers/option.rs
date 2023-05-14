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
}

impl<TValue> MapOrDefault for Option<TValue> {
    type Unwrapped = TValue;

    fn map_or_default<TMapped: Default>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> TMapped,
    ) -> TMapped {
        self.map(mapper).unwrap_or_default()
    }
}

pub trait ThenAnd {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped>;
}

impl ThenAnd for bool {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped> {
        self.then(mapper).flatten()
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
}

impl<TValue> Matches for Option<TValue> {
    type Unwrapped = TValue;

    fn matches(self, predicate: impl FnOnce(Self::Unwrapped) -> bool) -> bool {
        self.map(predicate) == Some(true)
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
