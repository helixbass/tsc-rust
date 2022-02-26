use std::collections::HashMap;

use crate::ModuleKind;

#[derive(Debug)]
pub struct ModeAwareCache<TValue> {
    underlying: HashMap<String, TValue>,
}

impl<TValue> ModeAwareCache<TValue> {
    pub fn get(&self, key: &str, mode: Option<ModuleKind>) -> Option<&TValue> {
        unimplemented!()
    }

    pub fn set(&self, key: String, mode: Option<ModuleKind>, value: TValue) {
        unimplemented!()
    }

    pub fn for_each<TCallback: FnMut(&TValue, &str, Option<ModuleKind>)>(&self, cb: TCallback) {
        unimplemented!()
    }
}

pub(crate) fn create_mode_aware_cache<TValue>() -> ModeAwareCache<TValue> {
    unimplemented!()
}
