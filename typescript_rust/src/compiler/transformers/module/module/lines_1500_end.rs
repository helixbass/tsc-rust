use gc::Gc;
use once_cell::unsync::Lazy;

use super::TransformModule;
use crate::{EmitHelper, Node, ReadonlyTextRange, ScopedEmitHelperBuilder};

impl TransformModule {
    pub(super) fn append_exports_of_import_equals_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ImportEqualsDeclaration*/
    ) /*: Statement[] | undefined */
    {
        unimplemented!()
    }

    pub(super) fn create_underscore_underscore_es_module(&self) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn create_export_expression(
        &self,
        _name: &Node,  /*Identifier*/
        _value: &Node, /*Expression*/
        _location: Option<&impl ReadonlyTextRange>,
        _live_binding: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn get_exports(
        &self,
        _name: &Node, /*Identifier*/
    ) -> Option<Vec<Gc<Node /*Identifier*/>>> {
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
