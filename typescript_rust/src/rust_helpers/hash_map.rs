use std::ops::Deref;
use std::{collections::HashMap, hash};

use gc::{Finalize, Gc, Trace};

#[derive(Clone, Debug, Trace, Finalize)]
pub struct GcHashMap<
    TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
    TValue: Trace + Finalize + Clone + 'static,
>(Gc<HashMap<TKey, TValue>>);

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Clone + Trace + Finalize + Clone + 'static,
    > GcHashMap<TKey, TValue>
{
    pub fn owned_values(&self) -> GcHashMapOwnedValues<TKey, TValue> {
        let cloned: GcHashMap<TKey, TValue> = self.clone();
        cloned.into()
    }
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Trace + Finalize + Clone + 'static,
    > Deref for GcHashMap<TKey, TValue>
{
    type Target = HashMap<TKey, TValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Trace + Finalize + Clone + 'static,
    > From<Gc<HashMap<TKey, TValue>>> for GcHashMap<TKey, TValue>
{
    fn from(value: Gc<HashMap<TKey, TValue>>) -> Self {
        Self(value)
    }
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Trace + Finalize + Clone + 'static,
    > From<HashMap<TKey, TValue>> for GcHashMap<TKey, TValue>
{
    fn from(value: HashMap<TKey, TValue>) -> Self {
        Gc::new(value).into()
    }
}

#[derive(Clone)]
pub struct GcHashMapOwnedValues<
    TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
    TValue: Clone + Trace + Finalize + Clone + 'static,
> {
    gc_hash_map: GcHashMap<TKey, TValue>,
    index: usize,
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Clone + Trace + Finalize + Clone + 'static,
    > Iterator for GcHashMapOwnedValues<TKey, TValue>
{
    type Item = TValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.gc_hash_map.len() {
            None
        } else {
            // TODO: is this expensive? Is there a way instead to hold eg a "live" `.values()`
            // iterator?
            let ret = self.gc_hash_map.values().nth(self.index).cloned().unwrap();
            self.index += 1;
            Some(ret)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.gc_hash_map.len() - 1 - self.index;
        (remaining, Some(remaining))
    }
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Clone + Trace + Finalize + Clone + 'static,
    > DoubleEndedIterator for GcHashMapOwnedValues<TKey, TValue>
{
    fn next_back(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Clone + Trace + Finalize + Clone + 'static,
    > ExactSizeIterator for GcHashMapOwnedValues<TKey, TValue>
{
}

impl<
        TKey: Trace + Finalize + Eq + hash::Hash + Clone + 'static,
        TValue: Clone + Trace + Finalize,
    > From<GcHashMap<TKey, TValue>> for GcHashMapOwnedValues<TKey, TValue>
{
    fn from(value: GcHashMap<TKey, TValue>) -> Self {
        Self {
            gc_hash_map: value,
            index: 0,
        }
    }
}
