pub mod collections {
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
        _map: HashMap<String, TValue>,
    }

    impl<TValue> Metadata<TValue> {
        pub fn get<'self_>(&'self_ self, key: &str) -> Option<&'self_ TValue> {
            unimplemented!()
        }

        pub fn set(&mut self, key: String, value: TValue) -> &mut Self {
            unimplemented!()
        }
    }
}
