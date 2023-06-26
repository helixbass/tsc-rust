pub mod collections {
    use std::{
        borrow::Cow,
        cell::Cell,
        cmp::Ordering,
        collections::HashMap,
        convert::{TryFrom, TryInto},
        io,
    };

    use gc::{Finalize, Gc, GcCell, Trace};
    use typescript_rust::{binary_search, Comparison};

    pub trait SortOptionsComparer<TKey>: Trace + Finalize {
        fn call(&self, a: &TKey, b: &TKey) -> Comparison;
    }

    pub struct SortOptions<TKey: 'static> {
        pub comparer: Gc<Box<dyn SortOptionsComparer<TKey>>>,
        pub sort: Option<SortOptionsSort>,
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum SortOptionsSort {
        Insertion,
        Comparison,
    }

    #[derive(Trace, Finalize)]
    pub struct SortedMap<TKey: Trace + Finalize + 'static, TValue: Trace + Finalize> {
        _comparer: Gc<Box<dyn SortOptionsComparer<TKey>>>,
        _keys: Vec<TKey>,
        _values: Vec<TValue>,
        _order: Option<Vec<usize>>,
        _version: usize,
        #[unsafe_ignore_trace]
        _copy_on_write: Cell<bool>,
    }

    impl<TKey: Trace + Finalize, TValue: Trace + Finalize> SortedMap<TKey, TValue> {
        pub fn new<TIterable: IntoIterator<Item = (TKey, TValue)>>(
            comparer: SortOptions<TKey>,
            iterable: Option<TIterable>,
        ) -> Self {
            let SortOptions {
                comparer: comparer_comparer,
                sort: comparer_sort,
            } = comparer;
            let mut ret = Self {
                _comparer: comparer_comparer,
                _order: if comparer_sort == Some(SortOptionsSort::Insertion) {
                    Some(vec![])
                } else {
                    None
                },
                _keys: vec![],
                _values: vec![],
                _version: 0,
                _copy_on_write: Cell::new(false),
            };
            if let Some(iterable) = iterable {
                for (key, value) in iterable {
                    ret.set(key, value);
                }
            }
            ret
        }

        pub fn len(&self) -> usize {
            self._keys.len()
        }

        pub fn is_empty(&self) -> bool {
            self._keys.is_empty()
        }

        fn _copy_on_write(&self) -> bool {
            self._copy_on_write.get()
        }

        fn set_copy_on_write(&self, _copy_on_write: bool) {
            self._copy_on_write.set(_copy_on_write);
        }

        pub fn has(&self, key: &TKey) -> bool {
            binary_search(
                &self._keys,
                key,
                |key: &TKey, _| key,
                |a: &TKey, b: &TKey| self._comparer.call(a, b),
                None,
            ) >= 0
        }

        pub fn get<'self_>(&'self_ self, key: &TKey) -> Option<&'self_ TValue> {
            let index = binary_search(
                &self._keys,
                key,
                |key: &TKey, _| key,
                |a: &TKey, b: &TKey| self._comparer.call(a, b),
                None,
            );
            if index >= 0 {
                let index: usize = index.try_into().unwrap();
                Some(&self._values[index])
            } else {
                None
            }
        }

        pub fn get_entry<'self_>(
            &'self_ self,
            key: &TKey,
        ) -> Option<(&'self_ TKey, &'self_ TValue)> {
            let index = binary_search(
                &self._keys,
                key,
                |key: &TKey, _| key,
                |a: &TKey, b: &TKey| self._comparer.call(a, b),
                None,
            );
            if index >= 0 {
                let index: usize = index.try_into().unwrap();
                Some((&self._keys[index], &self._values[index]))
            } else {
                None
            }
        }

        pub fn set(&mut self, key: TKey, value: TValue) -> &mut Self {
            let index = binary_search(
                &self._keys,
                &key,
                |key: &TKey, _| key,
                |a: &TKey, b: &TKey| self._comparer.call(a, b),
                None,
            );
            if index >= 0 {
                let index: usize = index.try_into().unwrap();
                self._values[index] = value;
            } else {
                self.write_preamble();
                insert_at(&mut self._keys, (!index).try_into().unwrap(), key);
                insert_at(&mut self._values, (!index).try_into().unwrap(), value);
                if let Some(_order) = self._order.as_mut() {
                    insert_at(_order, (!index).try_into().unwrap(), self._version);
                }
                self.write_post_script();
            }
            self
        }

        pub fn for_each(
            &self,
            mut callback: impl FnMut(&TValue, &TKey, &Self), /*, thisArg?: any*/
        ) {
            self.try_for_each(|value: &TValue, key: &TKey, self_: &Self| {
                Ok(callback(value, key, self_))
            })
            .unwrap()
        }

        pub fn try_for_each(
            &self,
            mut callback: impl FnMut(&TValue, &TKey, &Self) -> io::Result<()>, /*, thisArg?: any*/
        ) -> io::Result<()> {
            let keys = &self._keys;
            let values = &self._values;
            let indices = self.get_iteration_order();
            let version = self._version;
            self.set_copy_on_write(true);
            // try {
            let finally = || {
                if version == self._version {
                    self.set_copy_on_write(false);
                }
            };
            if let Some(indices) = indices {
                for i in indices {
                    callback(&values[i], &keys[i], self).map_err(|err| {
                        finally();
                        err
                    })?;
                }
            } else {
                for (i, key) in keys.into_iter().enumerate() {
                    callback(&values[i], key, self).map_err(|err| {
                        finally();
                        err
                    })?;
                }
            }
            // }
            // finally {
            finally();
            Ok(())
            // }
        }

        pub fn keys(&self) -> Keys<'_, TKey, TValue> {
            let indices = self.get_iteration_order();
            self.set_copy_on_write(true);
            Keys::new(&self._keys, indices, self._version, self)
        }

        pub fn entries(&self) -> Entries<'_, TKey, TValue> {
            let indices = self.get_iteration_order();
            self.set_copy_on_write(true);
            Entries::new(&self._keys, &self._values, indices, self._version, self)
        }

        fn write_preamble(&mut self) {
            // TODO: it looks like there'd need to be shared references to _keys/_values/_order for
            // this to be meaningful
            // if self._copy_on_write {
            //     self._keys = self._keys.clone();
            //     self._values = self._values.clone();
            //     if self._order.is_some() {
            //         self._order = self._order.clone();
            //     }
            //     self._copy_on_write = false;
            // }
        }

        fn write_post_script(&mut self) {
            self._version += 1;
        }

        fn get_iteration_order(&self) -> Option<Vec<usize>> {
            self._order.as_ref().map(|order| {
                let mut order_indices = order
                    .into_iter()
                    .enumerate()
                    .map(|(_, i)| i)
                    .copied()
                    .collect::<Vec<_>>();
                order_indices.sort_by(|a, b| {
                    let diff =
                        isize::try_from(order[*a]).unwrap() - isize::try_from(order[*b]).unwrap();
                    if diff > 0 {
                        Ordering::Greater
                    } else if diff < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                });
                order_indices
            })
        }
    }

    pub struct Keys<
        'sorted_map,
        TKey: Trace + Finalize + 'static,
        TValue: 'sorted_map + Trace + Finalize,
    > {
        _keys: &'sorted_map Vec<TKey>,
        indices: Option<Vec<usize>>,
        version: usize,
        sorted_map: &'sorted_map SortedMap<TKey, TValue>,
        current_index: usize,
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Keys<'sorted_map, TKey, TValue>
    {
        pub fn new(
            _keys: &'sorted_map Vec<TKey>,
            indices: Option<Vec<usize>>,
            version: usize,
            sorted_map: &'sorted_map SortedMap<TKey, TValue>,
        ) -> Self {
            Self {
                _keys,
                indices,
                version,
                sorted_map,
                current_index: 0,
            }
        }
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Iterator for Keys<'sorted_map, TKey, TValue>
    {
        type Item = &'sorted_map TKey;

        fn next(&mut self) -> Option<Self::Item> {
            if self.current_index >= self._keys.len() {
                return None;
            }
            let ret = if let Some(indices) = self.indices.as_ref() {
                &self._keys[indices[self.current_index]]
            } else {
                &self._keys[self.current_index]
            };
            self.current_index += 1;
            Some(ret)
        }
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Drop for Keys<'sorted_map, TKey, TValue>
    {
        fn drop(&mut self) {
            if self.version == self.sorted_map._version {
                self.sorted_map.set_copy_on_write(false);
            }
        }
    }

    pub struct Entries<
        'sorted_map,
        TKey: Trace + Finalize + 'static,
        TValue: 'sorted_map + Trace + Finalize,
    > {
        _keys: &'sorted_map Vec<TKey>,
        _values: &'sorted_map Vec<TValue>,
        indices: Option<Vec<usize>>,
        version: usize,
        sorted_map: &'sorted_map SortedMap<TKey, TValue>,
        current_index: usize,
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Entries<'sorted_map, TKey, TValue>
    {
        pub fn new(
            _keys: &'sorted_map Vec<TKey>,
            _values: &'sorted_map Vec<TValue>,
            indices: Option<Vec<usize>>,
            version: usize,
            sorted_map: &'sorted_map SortedMap<TKey, TValue>,
        ) -> Self {
            Self {
                _keys,
                _values,
                indices,
                version,
                sorted_map,
                current_index: 0,
            }
        }
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Iterator for Entries<'sorted_map, TKey, TValue>
    {
        type Item = (&'sorted_map TKey, &'sorted_map TValue);

        fn next(&mut self) -> Option<Self::Item> {
            if self.current_index >= self._keys.len() {
                return None;
            }
            let ret = if let Some(indices) = self.indices.as_ref() {
                (
                    &self._keys[indices[self.current_index]],
                    &self._values[indices[self.current_index]],
                )
            } else {
                (
                    &self._keys[self.current_index],
                    &self._values[self.current_index],
                )
            };
            self.current_index += 1;
            Some(ret)
        }
    }

    impl<
            'sorted_map,
            TKey: 'sorted_map + Trace + Finalize,
            TValue: 'sorted_map + Trace + Finalize,
        > Drop for Entries<'sorted_map, TKey, TValue>
    {
        fn drop(&mut self) {
            if self.version == self.sorted_map._version {
                self.sorted_map.set_copy_on_write(false);
            }
        }
    }

    pub fn insert_at<TItem>(array: &mut Vec<TItem>, index: usize, value: TItem) {
        if index == 0 {
            array.insert(0, value);
        } else if index == array.len() {
            array.push(value);
        } else {
            array.insert(index, value);
        }
    }

    // #[derive(Trace, Finalize)]
    pub struct Metadata<TValue: Trace + Finalize + 'static> {
        _parent: Option<Gc<GcCell<Metadata<TValue>>>>,
        _map: HashMap<String, TValue>,
        _version: usize,
        // #[unsafe_ignore_trace]
        _size: Cell<Option<usize>>,
        // #[unsafe_ignore_trace]
        _parent_version: Cell<Option<usize>>,
    }
    // TODO: did this to avoid a compiler overflow trying to resolve traits
    unsafe impl<TValue: Trace + Finalize + 'static> Trace for Metadata<TValue> {
        gc::custom_trace!(this, {
            mark(&this._parent);
            mark(&this._map);
        });
    }
    impl<TValue: Trace + Finalize + 'static> Finalize for Metadata<TValue> {}

    impl<TValue: Clone + Trace + Finalize + 'static> Metadata<TValue> {
        pub fn new(parent: Option<Gc<GcCell<Metadata<TValue>>>>) -> Self {
            Self {
                _parent: parent,
                _map: HashMap::new(),
                _version: 0,
                _size: Cell::new(None),
                _parent_version: Cell::new(None),
            }
        }

        pub fn size(&self) -> usize {
            if self._size.get().is_none()
                || matches!(
                    self._parent.clone(),
                    Some(_parent) if self._parent_version.get() != Some((*_parent).borrow()._version)
                )
            {
                self._size.set(Some(
                    self._map.len()
                        + self
                            ._parent
                            .clone()
                            .map_or(0, |_parent| (*_parent).borrow().size()),
                ));
                if let Some(_parent) = self._parent.clone() {
                    self._parent_version.set(Some((*_parent).borrow()._version));
                }
            }
            self._size.get().unwrap()
        }

        pub fn parent(&self) -> Option<Gc<GcCell<Metadata<TValue>>>> {
            self._parent.clone()
        }

        pub fn has(&self, key: &str) -> bool {
            self._map.contains_key(&*Self::_escape_key(key))
                || matches!(
                    self._parent.clone(),
                    Some(_parent) if (*_parent).borrow().has(key)
                )
        }

        pub fn get(&self, key: &str) -> Option<TValue> {
            let value = self
                ._map
                .get(&*Self::_escape_key(key))
                .cloned()
                .or_else(|| {
                    self._parent
                        .clone()
                        .and_then(|_parent| (*_parent).borrow().get(key))
                });
            value
        }

        pub fn set(&mut self, key: &str, value: TValue) -> &mut Self {
            self._map.insert(Self::_escape_key(key).into_owned(), value);
            self._size.set(None);
            self._version += 1;
            self
        }

        pub fn delete(&mut self, key: &str) -> bool {
            let escaped_key = Self::_escape_key(key);
            // TODO: it looks like the Typescript version would detect (but not delete) keys that
            // are on the parent (aka prototype)?
            if self._map.contains_key(&*escaped_key) {
                self._map.remove(&*escaped_key);
                self._size.set(None);
                self._version += 1;
                return true;
            }
            false
        }

        pub fn clear(&mut self) {
            self._map = HashMap::new();
            self._size.set(None);
            self._version += 1;
        }

        pub fn for_each<TCallback: FnMut(&TValue, &str, &Self)>(&self, mut callback: TCallback) {
            for (key, value) in &self._map {
                callback(value, &Self::_unescape_key(key), self)
            }
            if let Some(_parent) = self._parent.clone() {
                (*_parent).borrow().for_each(callback);
            }
        }

        fn _escape_key(text: &str) -> Cow<'_, str> {
            if
            /*text.len() >= 2 &&*/
            text.starts_with("__") {
                format!("_{text}").into()
            } else {
                text.into()
            }
        }

        fn _unescape_key(text: &str) -> &str {
            if
            /*text.len() >= 3 &&*/
            text.starts_with("___") {
                &text[1..]
            } else {
                text
            }
        }
    }
}
