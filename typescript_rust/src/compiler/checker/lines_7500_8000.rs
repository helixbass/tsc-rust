use std::borrow::Borrow;

use gc::{Finalize, Gc, Trace};

use crate::{
    Node, NodeArrayOrVec, SignatureKind, StrOrRcNode, Symbol, SymbolFlags, SyntaxKind, Type,
};

use super::SymbolTableToDeclarationStatements;

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_export_specifier(
        &self,
        local_name: &str,
        target_name: &str,
        specifier: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_representable_as_function_namespace_merge(
        &self,
        type_to_serialize: &Type,
        host_symbol: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn make_serialize_property_symbol(
        &self,
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> MakeSerializePropertySymbol {
        MakeSerializePropertySymbol::new(create_property, method_kind, use_accessors)
    }

    pub(super) fn serialize_property_symbol_for_interface(
        &self,
        p: &Symbol,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
        output_kind: SyntaxKind,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_base_type(
        &self,
        t: &Type,
        static_type: &Type,
        root_name: &str,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: &Type,
        flags: SymbolFlags,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_implemented_type(&self, t: &Type) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_unused_name(
        &self,
        input: &str,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn get_internal_symbol_name(&self, symbol: &Symbol, local_name: &str) -> String {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbol {
    create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
    method_kind: SyntaxKind,
    use_accessors: bool,
}

impl MakeSerializePropertySymbol {
    pub(super) fn new(
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> Self {
        Self {
            create_property,
            method_kind,
            use_accessors,
        }
    }

    pub(super) fn call(
        &self,
        p: &Symbol,
        is_static: bool,
        base_type: Option<&Type>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }
}

pub(super) trait MakeSerializePropertySymbolCreateProperty: Trace + Finalize {
    fn call(
        &self,
        decorators: Option<NodeArrayOrVec /*Decorator*/>,
        modifiers: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node>;
}
