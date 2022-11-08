pub mod collections {
    use std::borrow::Cow;
    use std::cell::{Cell, RefCell};
    use std::collections::HashMap;
    use std::rc::Rc;
    use typescript_rust::Comparison;

    pub struct SortOptions<TKey> {
        pub comparer: Rc<dyn Fn(&TKey, &TKey) -> Comparison>,
        pub sort: Option<SortOptionsSort>,
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum SortOptionsSort {
        Insertion,
        Comparison,
    }

    pub struct SortedMap<TKey, TValue> {
        _comparer: Rc<dyn Fn(&TKey, &TKey) -> Comparison>,
        _keys: Vec<TKey>,
        _values: Vec<TValue>,
        _order: Option<Vec<usize>>,
        _version: usize,
        _copy_on_write: bool,
    }

    impl<TKey, TValue> SortedMap<TKey, TValue> {
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
                _copy_on_write: false,
            };
            if let Some(iterable) = iterable {
                for (key, value) in iterable {
                    ret.set(key, value);
                }
            }
            ret
        }

        pub fn set(&mut self, key: TKey, value: TValue) -> &mut Self {
            unimplemented!()
        }

        pub fn keys(&self) -> Keys<'_, TKey, TValue> {
            unimplemented!()
        }
    }

    pub struct Keys<'sorted_map, TKey: 'sorted_map, TValue: 'sorted_map> {
        sorted_map: &'sorted_map SortedMap<TKey, TValue>,
    }

    impl<'sorted_map, TKey: 'sorted_map, TValue: 'sorted_map> Iterator
        for Keys<'sorted_map, TKey, TValue>
    {
        type Item = &'sorted_map TKey;

        fn next(&mut self) -> Option<Self::Item> {
            unimplemented!()
        }
    }

    pub struct Metadata<TValue> {
        _parent: Option<Rc<RefCell<Metadata<TValue>>>>,
        _map: HashMap<String, TValue>,
        _version: usize,
        _size: Cell<Option<usize>>,
        _parent_version: Cell<Option<usize>>,
    }

    impl<TValue: Clone> Metadata<TValue> {
        pub fn new(parent: Option<Rc<RefCell<Metadata<TValue>>>>) -> Self {
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

        pub fn parent(&self) -> Option<Rc<RefCell<Metadata<TValue>>>> {
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
