use gc::Gc;
use once_cell::unsync::Lazy;

use super::TransformModule;
use crate::{EmitHelper, Node, ScopedEmitHelperBuilder};

impl TransformModule {
    pub(super) fn create_underscore_underscore_es_module(&self) -> Gc<Node> {
        unimplemented!()
    }
}

thread_local! {
    static _dynamic_import_umd_helper: Lazy<Gc<EmitHelper>> = Lazy::new(||
        ScopedEmitHelperBuilder::default()
            .name("typescript:dynamicimport-sync-require")
            .text("\n            var __syncRequire = typeof module === \"object\" && typeof module.exports === \"object\";".to_owned())
            .build().unwrap().into()
    )
}
pub(super) fn dynamic_import_umd_helper() -> Gc<EmitHelper> {
    _dynamic_import_umd_helper
        .with(|dynamic_import_umd_helper| (**dynamic_import_umd_helper).clone())
}
