use std::collections::HashMap;

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, AttributeArgs, Block,
    Data::{Enum, Struct},
    DataEnum, DeriveInput, Error, Expr, ExprArray, Fields, FieldsNamed, FnArg, ItemFn, Pat,
    ReturnType, Token,
};

#[derive(Debug, FromMeta)]
struct AstTypeArgs {
    #[darling(default)]
    ancestors: Option<String>,
    #[darling(default)]
    impl_from: Option<bool>,
    #[darling(default)]
    interfaces: Option<String>,
}

impl AstTypeArgs {
    fn ancestors_vec(&self, ast_type_name: &Ident) -> Vec<String> {
        let mut vec = self.ancestors.as_ref().map_or_else(
            || vec![],
            |ancestors_str| {
                ancestors_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect()
            },
        );
        if ast_type_name.to_string() != "Node" {
            vec.push("Node".to_string());
        }
        vec
    }

    fn should_impl_from(&self) -> bool {
        self.impl_from.unwrap_or(true)
    }

    fn interfaces_vec(&self) -> Vec<String> {
        let mut vec = vec!["ReadonlyTextRange".to_string(), "NodeInterface".to_string()];
        if let Some(interfaces_str) = self.interfaces.as_ref() {
            vec.append(
                &mut interfaces_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect(),
            );
        }
        vec
    }
}

fn get_ast_struct_interface_impl(
    interface_name: &str,
    first_field_name: &Ident,
    ast_type_name: &Ident,
    should_impl_from: bool,
) -> TokenStream2 {
    match interface_name {
        "NodeInterface" => {
            // TODO: expose .maybe_arena_id() method and do a
            // debug_assert!(self.maybe_arena_id().is_none()
            // (ie sanity-checking that we aren't trying to allocate
            // something that's already been arena-allocated) here
            // (aad in BaseNode's impl of NodeInterface::alloc())?
            let alloc_method = if should_impl_from {
                quote! {
                    fn alloc(self, arena: &crate::AllArenas) -> ::id_arena::Id<crate::Node> {
                        let id = crate::HasArena::alloc_node(arena, crate::Node::from(self));
                        crate::HasArena::node(arena, id).set_arena_id(id);
                        id
                    }
                }
            } else {
                quote! {
                    fn alloc(self, _arena: &crate::AllArenas) -> ::id_arena::Id<crate::Node> {
                        unreachable!()
                    }
                }
            };

            quote! {
                impl crate::NodeInterface for #ast_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::Node> {
                        self.#first_field_name.arena_id()
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::Node>) {
                        self.#first_field_name.set_arena_id(id)
                    }

                    #alloc_method

                    fn base_node(&self) -> &crate::BaseNode {
                        self.#first_field_name.base_node()
                    }

                    fn kind(&self) -> crate::SyntaxKind {
                        self.#first_field_name.kind()
                    }

                    fn flags(&self) -> crate::NodeFlags {
                        self.#first_field_name.flags()
                    }

                    fn set_flags(&self, flags: crate::NodeFlags) {
                        self.#first_field_name.set_flags(flags)
                    }

                    fn modifier_flags_cache(&self) -> crate::ModifierFlags {
                        self.#first_field_name.modifier_flags_cache()
                    }

                    fn set_modifier_flags_cache(&self, flags: crate::ModifierFlags) {
                        self.#first_field_name.set_modifier_flags_cache(flags)
                    }

                    fn transform_flags(&self) -> crate::TransformFlags {
                        self.#first_field_name.transform_flags()
                    }

                    fn set_transform_flags(&self, flags: crate::TransformFlags) {
                        self.#first_field_name.set_transform_flags(flags)
                    }

                    fn add_transform_flags(&self, flags: crate::TransformFlags) {
                        self.#first_field_name.add_transform_flags(flags)
                    }

                    fn maybe_decorators(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        self.#first_field_name.maybe_decorators()
                    }

                    fn set_decorators(&self, decorators: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        self.#first_field_name.set_decorators(decorators)
                    }

                    fn maybe_modifiers(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        self.#first_field_name.maybe_modifiers()
                    }

                    fn set_modifiers(&self, modifiers: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        self.#first_field_name.set_modifiers(modifiers)
                    }

                    fn maybe_id(&self) -> ::std::option::Option<crate::NodeId> {
                        self.#first_field_name.maybe_id()
                    }

                    fn id(&self) -> crate::NodeId {
                        self.#first_field_name.id()
                    }

                    fn set_id(&self, id: crate::NodeId) {
                        self.#first_field_name.set_id(id)
                    }

                    fn set_id_override(&self, id_override: ::id_arena::Id<::std::boxed::Box<dyn crate::NodeIdOverride>>) {
                        self.#first_field_name.set_id_override(id_override)
                    }

                    fn maybe_parent(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_parent()
                    }

                    fn parent(&self) -> ::id_arena::Id<crate::Node> {
                        self.#first_field_name.parent()
                    }

                    fn set_parent(&self, parent: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_parent(parent)
                    }

                    fn maybe_original(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_original()
                    }

                    fn set_original(&self, original: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_original(original)
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_symbol()
                    }

                    fn symbol(&self) -> ::id_arena::Id<crate::Symbol> {
                        self.#first_field_name.symbol()
                    }

                    fn set_symbol(&self, symbol: ::id_arena::Id<crate::Symbol>) {
                        self.#first_field_name.set_symbol(symbol);
                    }

                    fn set_symbol_override(&self, symbol_override: ::id_arena::Id<::std::boxed::Box<dyn crate::NodeSymbolOverride>>) {
                        self.#first_field_name.set_symbol_override(symbol_override)
                    }

                    fn maybe_locals(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.maybe_locals()
                    }

                    fn maybe_locals_mut(&self) -> ::debug_cell::RefMut<::std::option::Option<::id_arena::Id<crate::SymbolTable>>> {
                        self.#first_field_name.maybe_locals_mut()
                    }

                    fn locals(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        self.#first_field_name.locals()
                    }

                    fn locals_mut(&self) -> ::debug_cell::RefMut<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.locals_mut()
                    }

                    fn set_locals(&self, locals: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        self.#first_field_name.set_locals(locals)
                    }

                    fn maybe_next_container(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_next_container()
                    }

                    fn set_next_container(&self, next_container: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_next_container(next_container)
                    }

                    fn maybe_local_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_local_symbol()
                    }

                    fn set_local_symbol(&self, local_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_local_symbol(local_symbol)
                    }

                    fn maybe_emit_node(&self) -> ::std::option::Option<::id_arena::Id<crate::EmitNode>> {
                        self.#first_field_name.maybe_emit_node()
                    }

                    fn set_emit_node(&self, emit_node: ::std::option::Option<::id_arena::Id<crate::EmitNode>>) {
                        self.#first_field_name.set_emit_node(emit_node)
                    }

                    fn maybe_contextual_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_contextual_type()
                    }

                    fn set_contextual_type(&self, contextual_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_contextual_type(contextual_type)
                    }

                    fn maybe_inference_context(&self) -> ::std::option::Option<::id_arena::Id<crate::InferenceContext>> {
                        self.#first_field_name.maybe_inference_context()
                    }

                    fn set_inference_context(&self, inference_context: ::std::option::Option<::id_arena::Id<crate::InferenceContext>>) {
                        self.#first_field_name.set_inference_context(inference_context)
                    }

                    fn maybe_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        self.#first_field_name.maybe_flow_node()
                    }

                    fn set_flow_node(&self, flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        self.#first_field_name.set_flow_node(flow_node)
                    }

                    fn maybe_js_doc(&self) -> ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>> {
                        self.#first_field_name.maybe_js_doc()
                    }

                    fn set_js_doc(&self, js_doc: ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>) {
                        self.#first_field_name.set_js_doc(js_doc)
                    }

                    fn maybe_js_doc_cache(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        self.#first_field_name.maybe_js_doc_cache()
                    }

                    fn set_js_doc_cache(&self, js_doc_cache: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Node>>>>) {
                        self.#first_field_name.set_js_doc_cache(js_doc_cache)
                    }

                    fn maybe_intersects_change(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_intersects_change()
                    }

                    fn set_intersects_change(&self, intersects_change: ::std::option::Option<bool>) {
                        self.#first_field_name.set_intersects_change(intersects_change)
                    }
                }
            }
        }
        "ReadonlyTextRange" => {
            quote! {
                impl crate::ReadonlyTextRange for #ast_type_name {
                    fn pos(&self) -> isize {
                        self.#first_field_name.pos()
                    }

                    fn set_pos(&self, pos: isize) {
                        self.#first_field_name.set_pos(pos);
                    }

                    fn end(&self) -> isize {
                        self.#first_field_name.end()
                    }

                    fn set_end(&self, end: isize) {
                        self.#first_field_name.set_end(end);
                    }
                }
            }
        }
        "NamedDeclarationInterface" => {
            quote! {
                impl crate::NamedDeclarationInterface for #ast_type_name {
                    fn maybe_name(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_name()
                    }

                    fn name(&self) -> ::id_arena::Id<crate::Node> {
                        self.#first_field_name.name()
                    }

                    fn set_name(&mut self, name: ::id_arena::Id<crate::Node>) {
                        self.#first_field_name.set_name(name);
                    }
                }
            }
        }
        "HasTypeParametersInterface" => {
            quote! {
                impl crate::HasTypeParametersInterface for #ast_type_name {
                    fn maybe_type_parameters(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        self.#first_field_name.maybe_type_parameters()
                    }

                    fn set_type_parameters(&self, type_parameters: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        self.#first_field_name.set_type_parameters(type_parameters);
                    }
                }
            }
        }
        "HasInitializerInterface" => {
            quote! {
                impl crate::HasInitializerInterface for #ast_type_name {
                    fn maybe_initializer(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_initializer()
                    }

                    fn set_initializer(&mut self, initializer: ::id_arena::Id<crate::Node>) {
                        self.#first_field_name.set_initializer(initializer);
                    }
                }
            }
        }
        "BindingLikeDeclarationInterface" => {
            quote! {
                impl crate::BindingLikeDeclarationInterface for #ast_type_name {}
            }
        }
        "HasTypeInterface" => {
            quote! {
                impl crate::HasTypeInterface for #ast_type_name {
                    fn maybe_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_type()
                    }

                    fn set_type(&mut self, type_: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_type(type_);
                    }
                }
            }
        }
        "VariableLikeDeclarationInterface" => {
            quote! {
                impl crate::VariableLikeDeclarationInterface for #ast_type_name {}
            }
        }
        "LiteralLikeNodeInterface" => {
            quote! {
                impl crate::LiteralLikeNodeInterface for #ast_type_name {
                    fn text(&self) -> ::std::cell::Ref<String> {
                        self.#first_field_name.text()
                    }

                    fn set_text(&self, text: String) {
                        self.#first_field_name.set_text(text)
                    }

                    fn is_unterminated(&self) -> Option<bool> {
                        self.#first_field_name.is_unterminated()
                    }

                    fn set_is_unterminated(&self, is_unterminated: Option<bool>) {
                        self.#first_field_name.set_is_unterminated(is_unterminated);
                    }

                    fn has_extended_unicode_escape(&self) -> Option<bool> {
                        self.#first_field_name.has_extended_unicode_escape()
                    }

                    fn set_has_extended_unicode_escape(&self, has_extended_unicode_escape: Option<bool>) {
                        self.#first_field_name.set_has_extended_unicode_escape(has_extended_unicode_escape);
                    }
                }
            }
        }
        "TemplateLiteralLikeNodeInterface" => {
            quote! {
                impl crate::TemplateLiteralLikeNodeInterface for #ast_type_name {
                    fn maybe_raw_text(&self) -> ::std::option::Option<&str> {
                        self.#first_field_name.maybe_raw_text()
                    }

                    fn maybe_template_flags(&self) -> ::std::option::Option<crate::TokenFlags> {
                        self.#first_field_name.maybe_template_flags()
                    }
                }
            }
        }
        "GenericNamedDeclarationInterface" => {
            quote! {
                impl crate::GenericNamedDeclarationInterface for #ast_type_name {}
            }
        }
        "SignatureDeclarationInterface" => {
            quote! {
                impl crate::SignatureDeclarationInterface for #ast_type_name {
                    fn parameters(&self) -> ::id_arena::Id<crate::NodeArray> {
                        self.#first_field_name.parameters()
                    }
                }
            }
        }
        "FunctionLikeDeclarationInterface" => {
            quote! {
                impl crate::FunctionLikeDeclarationInterface for #ast_type_name {
                    fn maybe_body(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_body()
                    }

                    fn maybe_asterisk_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_asterisk_token()
                    }

                    fn maybe_exclamation_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_exclamation_token()
                    }

                    fn set_exclamation_token(&self, exclamation_token: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_exclamation_token(exclamation_token)
                    }

                    fn maybe_end_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        self.#first_field_name.maybe_end_flow_node()
                    }

                    fn set_end_flow_node(&self, end_flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        self.#first_field_name.set_end_flow_node(end_flow_node)
                    }

                    fn maybe_return_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        self.#first_field_name.maybe_return_flow_node()
                    }

                    fn set_return_flow_node(&self, return_flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        self.#first_field_name.set_return_flow_node(return_flow_node)
                    }
                }
            }
        }
        "JSDocTagInterface" => {
            quote! {
                impl crate::JSDocTagInterface for #ast_type_name {
                    fn tag_name(&self) -> ::id_arena::Id<crate::Node> {
                        self.#first_field_name.tag_name()
                    }

                    fn maybe_comment(&self) -> ::std::option::Option<&crate::StringOrNodeArray> {
                        self.#first_field_name.maybe_comment()
                    }
                }
            }
        }
        "InterfaceOrClassLikeDeclarationInterface" => {
            quote! {
                impl crate::InterfaceOrClassLikeDeclarationInterface for #ast_type_name {
                    fn maybe_heritage_clauses(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        self.#first_field_name.maybe_heritage_clauses()
                    }
                }
            }
        }
        "ClassLikeDeclarationInterface" => {
            quote! {
                impl crate::ClassLikeDeclarationInterface for #ast_type_name {
                    fn members(&self) -> ::id_arena::Id<crate::NodeArray> {
                        self.#first_field_name.members()
                    }
                }
            }
        }
        "UnparsedSectionInterface" => {
            quote! {
                impl crate::UnparsedSectionInterface for #ast_type_name {
                    fn maybe_data(&self) -> ::std::option::Option<&str> {
                        self.#first_field_name.maybe_data()
                    }
                }
            }
        }
        "HasQuestionTokenInterface" => {
            quote! {
                impl crate::HasQuestionTokenInterface for #ast_type_name {
                    fn maybe_question_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_question_token()
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

fn get_ast_enum_interface_impl(
    interface_name: &str,
    variant_names: &[&Ident],
    ast_type_name: &Ident,
    should_impl_from: bool,
) -> TokenStream2 {
    match interface_name {
        "NodeInterface" => {
            let alloc_method = if should_impl_from {
                quote! {
                    fn alloc(self, arena: &crate::AllArenas) -> ::id_arena::Id<crate::Node> {
                        let id = crate::HasArena::alloc_node(arena, crate::Node::from(self));
                        crate::HasArena::node(arena, id).set_arena_id(id);
                        id
                    }
                }
            } else {
                quote! {
                    fn alloc(self, _arena: &crate::AllArenas) -> ::id_arena::Id<crate::Node> {
                        unreachable!()
                    }
                }
            };

            quote! {
                impl crate::NodeInterface for #ast_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.arena_id()),*
                        }
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_arena_id(id)),*
                        }
                    }

                    #alloc_method

                    fn base_node(&self) -> &crate::BaseNode {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.base_node()),*
                        }
                    }

                    fn kind(&self) -> crate::SyntaxKind {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.kind()),*
                        }
                    }

                    fn flags(&self) -> crate::NodeFlags {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.flags()),*
                        }
                    }

                    fn set_flags(&self, flags: crate::NodeFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_flags(flags)),*
                        }
                    }

                    fn modifier_flags_cache(&self) -> crate::ModifierFlags {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.modifier_flags_cache()),*
                        }
                    }

                    fn set_modifier_flags_cache(&self, flags: crate::ModifierFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_modifier_flags_cache(flags)),*
                        }
                    }

                    fn transform_flags(&self) -> crate::TransformFlags {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.transform_flags()),*
                        }
                    }

                    fn set_transform_flags(&self, flags: crate::TransformFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_transform_flags(flags)),*
                        }
                    }

                    fn add_transform_flags(&self, flags: crate::TransformFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.add_transform_flags(flags)),*
                        }
                    }

                    fn maybe_decorators(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_decorators()),*
                        }
                    }

                    fn set_decorators(&self, decorators: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_decorators(decorators)),*
                        }
                    }

                    fn maybe_modifiers(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_modifiers()),*
                        }
                    }

                    fn set_modifiers(&self, modifiers: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_modifiers(modifiers)),*
                        }
                    }

                    fn maybe_id(&self) -> ::std::option::Option<crate::NodeId> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_id()),*
                        }
                    }

                    fn id(&self) -> crate::NodeId {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.id()),*
                        }
                    }

                    fn set_id(&self, id: crate::NodeId) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_id(id)),*
                        }
                    }

                    fn set_id_override(&self, id_override: ::id_arena::Id<::std::boxed::Box<dyn crate::NodeIdOverride>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_id_override(id_override)),*
                        }
                    }

                    fn maybe_parent(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_parent()),*
                        }
                    }

                    fn parent(&self) -> ::id_arena::Id<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.parent()),*
                        }
                    }

                    fn set_parent(&self, parent: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_parent(parent)),*
                        }
                    }

                    fn maybe_original(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_original()),*
                        }
                    }

                    fn set_original(&self, original: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_original(original)),*
                        }
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_symbol()),*
                        }
                    }

                    fn symbol(&self) -> ::id_arena::Id<crate::Symbol> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.symbol()),*
                        }
                    }

                    fn set_symbol(&self, symbol: ::id_arena::Id<crate::Symbol>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_symbol(symbol)),*
                        }
                    }

                    fn set_symbol_override(&self, symbol_override: ::id_arena::Id<::std::boxed::Box<dyn crate::NodeSymbolOverride>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_symbol_override(symbol_override)),*
                        }
                    }

                    fn maybe_locals(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_locals()),*
                        }
                    }

                    fn maybe_locals_mut(&self) -> ::debug_cell::RefMut<::std::option::Option<::id_arena::Id<crate::SymbolTable>>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_locals_mut()),*
                        }
                    }

                    fn locals(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.locals()),*
                        }
                    }

                    fn locals_mut(&self) -> ::debug_cell::RefMut<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.locals_mut()),*
                        }
                    }

                    fn set_locals(&self, locals: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_locals(locals)),*
                        }
                    }

                    fn maybe_next_container(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_next_container()),*
                        }
                    }

                    fn set_next_container(&self, next_container: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_next_container(next_container)),*
                        }
                    }

                    fn maybe_local_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_local_symbol()),*
                        }
                    }

                    fn set_local_symbol(&self, local_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_local_symbol(local_symbol)),*
                        }
                    }

                    fn maybe_emit_node(&self) -> ::std::option::Option<::id_arena::Id<crate::EmitNode>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_emit_node()),*
                        }
                    }

                    fn set_emit_node(&self, emit_node: ::std::option::Option<::id_arena::Id<crate::EmitNode>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_emit_node(emit_node)),*
                        }
                    }

                    fn maybe_contextual_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_contextual_type()),*
                        }
                    }

                    fn set_contextual_type(&self, contextual_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_contextual_type(contextual_type)),*
                        }
                    }

                    fn maybe_inference_context(&self) -> ::std::option::Option<::id_arena::Id<crate::InferenceContext>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_inference_context()),*
                        }
                    }

                    fn set_inference_context(&self, inference_context: ::std::option::Option<::id_arena::Id<crate::InferenceContext>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_inference_context(inference_context)),*
                        }
                    }

                    fn maybe_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_flow_node()),*
                        }
                    }

                    fn set_flow_node(&self, flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_flow_node(flow_node)),*
                        }
                    }

                    fn maybe_js_doc(&self) -> ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_js_doc()),*
                        }
                    }

                    fn set_js_doc(&self, js_doc: ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_js_doc(js_doc)),*
                        }
                    }

                    fn maybe_js_doc_cache(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_js_doc_cache()),*
                        }
                    }

                    fn set_js_doc_cache(&self, js_doc_cache: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Node>>>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_js_doc_cache(js_doc_cache)),*
                        }
                    }

                    fn maybe_intersects_change(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_intersects_change()),*
                        }
                    }

                    fn set_intersects_change(&self, intersects_change: ::std::option::Option<bool>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_intersects_change(intersects_change)),*
                        }
                    }
                }
            }
        }
        "ReadonlyTextRange" => {
            quote! {
                impl crate::ReadonlyTextRange for #ast_type_name {
                    fn pos(&self) -> isize {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.pos()),*
                        }
                    }

                    fn set_pos(&self, pos: isize) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_pos(pos)),*
                        }
                    }

                    fn end(&self) -> isize {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.end()),*
                        }
                    }

                    fn set_end(&self, end: isize) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_end(end)),*
                        }
                    }
                }
            }
        }
        "LiteralLikeNodeInterface" => {
            quote! {
                impl crate::LiteralLikeNodeInterface for #ast_type_name {
                    fn text(&self) -> ::std::cell::Ref<String> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.text()),*
                        }
                    }

                    fn set_text(&self, text: String) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_text(text)),*
                        }
                    }

                    fn is_unterminated(&self) -> Option<bool> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.is_unterminated()),*
                        }
                    }

                    fn set_is_unterminated(&self, is_unterminated: Option<bool>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_is_unterminated(is_unterminated)),*
                        }
                    }

                    fn has_extended_unicode_escape(&self) -> Option<bool> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.has_extended_unicode_escape()),*
                        }
                    }

                    fn set_has_extended_unicode_escape(&self, has_extended_unicode_escape: Option<bool>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_has_extended_unicode_escape(has_extended_unicode_escape)),*
                        }
                    }
                }
            }
        }
        "NamedDeclarationInterface" => {
            quote! {
                impl crate::NamedDeclarationInterface for #ast_type_name {
                    fn maybe_name(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_name()),*
                        }
                    }

                    fn name(&self) -> ::id_arena::Id<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.name()),*
                        }
                    }

                    fn set_name(&mut self, name: ::id_arena::Id<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_name(name)),*
                        }
                    }
                }
            }
        }
        "HasTypeParametersInterface" => {
            quote! {
                impl crate::HasTypeParametersInterface for #ast_type_name {
                    fn maybe_type_parameters(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_type_parameters()),*
                        }
                    }

                    fn set_type_parameters(&self, type_parameters: ::std::option::Option<::id_arena::Id<crate::NodeArray>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_type_parameters(type_parameters)),*
                        }
                    }
                }
            }
        }
        "TemplateLiteralLikeNodeInterface" => {
            quote! {
                impl crate::TemplateLiteralLikeNodeInterface for #ast_type_name {
                    fn maybe_raw_text(&self) -> ::std::option::Option<&str> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_raw_text()),*
                        }
                    }

                    fn maybe_template_flags(&self) -> ::std::option::Option<crate::TokenFlags> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_template_flags()),*
                        }
                    }
                }
            }
        }
        "GenericNamedDeclarationInterface" => {
            quote! {
                impl crate::GenericNamedDeclarationInterface for #ast_type_name {}
            }
        }
        "SignatureDeclarationInterface" => {
            quote! {
                impl crate::SignatureDeclarationInterface for #ast_type_name {
                    fn parameters(&self) -> ::id_arena::Id<crate::NodeArray> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.parameters()),*
                        }
                    }
                }
            }
        }
        "FunctionLikeDeclarationInterface" => {
            quote! {
                impl crate::FunctionLikeDeclarationInterface for #ast_type_name {
                    fn maybe_body(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_body()),*
                        }
                    }

                    fn maybe_asterisk_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_asterisk_token()),*
                        }
                    }

                    fn maybe_exclamation_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_exclamation_token()),*
                        }
                    }

                    fn set_exclamation_token(&self, exclamation_token: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_exclamation_token(exclamation_token)),*
                        }
                    }

                    fn maybe_end_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_end_flow_node()),*
                        }
                    }

                    fn set_end_flow_node(&self, end_flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_end_flow_node(end_flow_node)),*
                        }
                    }

                    fn maybe_return_flow_node(&self) -> ::std::option::Option<::id_arena::Id<crate::FlowNode>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_return_flow_node()),*
                        }
                    }

                    fn set_return_flow_node(&self, return_flow_node: ::std::option::Option<::id_arena::Id<crate::FlowNode>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_return_flow_node(return_flow_node)),*
                        }
                    }
                }
            }
        }
        "HasTypeInterface" => {
            quote! {
                impl crate::HasTypeInterface for #ast_type_name {
                    fn maybe_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_type()),*
                        }
                    }

                    fn set_type(&mut self, type_: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_type(type_)),*
                        }
                    }
                }
            }
        }
        "JSDocTagInterface" => {
            quote! {
                impl crate::JSDocTagInterface for #ast_type_name {
                    fn tag_name(&self) -> ::id_arena::Id<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.tag_name()),*
                        }
                    }

                    fn maybe_comment(&self) -> ::std::option::Option<&crate::StringOrNodeArray> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_comment()),*
                        }
                    }
                }
            }
        }
        "InterfaceOrClassLikeDeclarationInterface" => {
            quote! {
                impl crate::InterfaceOrClassLikeDeclarationInterface for #ast_type_name {
                    fn maybe_heritage_clauses(&self) -> ::std::option::Option<::id_arena::Id<crate::NodeArray>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_heritage_clauses()),*
                        }
                    }
                }
            }
        }
        "ClassLikeDeclarationInterface" => {
            quote! {
                impl crate::ClassLikeDeclarationInterface for #ast_type_name {
                    fn members(&self) -> ::id_arena::Id<crate::NodeArray> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.members()),*
                        }
                    }
                }
            }
        }
        "UnparsedSectionInterface" => {
            quote! {
                impl crate::UnparsedSectionInterface for #ast_type_name {
                    fn maybe_data(&self) -> ::std::option::Option<&str> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_data()),*
                        }
                    }
                }
            }
        }
        "HasQuestionTokenInterface" => {
            quote! {
                impl crate::HasQuestionTokenInterface for #ast_type_name {
                    fn maybe_question_token(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_question_token()),*
                        }
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

#[proc_macro_attribute]
pub fn ast_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: ast_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let args = match AstTypeArgs::from_list(&attr_args) {
        Ok(args) => args,
        Err(error) => {
            return TokenStream::from(error.write_errors());
        }
    };

    let node_interface_and_readonly_text_range_implementation = match data {
        Struct(struct_) => {
            let first_field_name = match struct_.fields {
                Fields::Named(FieldsNamed { named, .. }) => named
                    .iter()
                    .nth(0)
                    .expect("Expected at least one struct field")
                    .ident
                    .clone()
                    .expect("Expected ident"),
                _ => panic!("Expected named fields"),
            };

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl = get_ast_struct_interface_impl(
                    &interface,
                    &first_field_name,
                    &ast_type_name,
                    args.should_impl_from(),
                );
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        Enum(DataEnum { variants, .. }) => {
            let variant_names = variants
                .iter()
                .map(|variant| &variant.ident)
                .collect::<Vec<_>>();

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl = get_ast_enum_interface_impl(
                    &interface,
                    &variant_names,
                    &ast_type_name,
                    args.should_impl_from(),
                );
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        _ => panic!("Expected struct or enum"),
    };

    let into_implementations = if args.should_impl_from() {
        let mut construct_variant = quote! {
            concrete
        };
        let mut previous_variant_name = ast_type_name.clone();
        let mut into_implementations = quote! {};
        for ancestor in args.ancestors_vec(&ast_type_name) {
            let ancestor_ident = Ident::new(&ancestor, previous_variant_name.span());
            construct_variant = quote! {
                crate::#ancestor_ident::#previous_variant_name(#construct_variant)
            };
            into_implementations = quote! {
                #into_implementations

                impl ::std::convert::From<#ast_type_name> for crate::#ancestor_ident {
                    fn from(concrete: #ast_type_name) -> Self {
                        #construct_variant
                    }
                }
            };
            previous_variant_name = ancestor_ident;
        }

        quote! {
            #into_implementations
        }
    } else {
        quote! {}
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #[derive(Clone)]
        #item_as_proc_macro2_token_stream

        #node_interface_and_readonly_text_range_implementation

        #into_implementations
    }
    .into()
}

#[derive(Debug, FromMeta)]
struct TypeTypeArgs {
    #[darling(default)]
    ancestors: Option<String>,
    #[darling(default)]
    impl_from: Option<bool>,
    #[darling(default)]
    interfaces: Option<String>,
}

impl TypeTypeArgs {
    fn ancestors_vec(&self) -> Vec<String> {
        let mut vec = self.ancestors.as_ref().map_or_else(
            || vec![],
            |ancestors_str| {
                ancestors_str
                    .split(',')
                    .map(|chunk| chunk.trim().to_string())
                    .collect()
            },
        );
        vec.push("Type".to_string());
        vec
    }

    fn should_impl_from(&self) -> bool {
        self.impl_from.unwrap_or(true)
    }

    fn interfaces_vec(&self) -> Vec<String> {
        let mut vec = vec!["TypeInterface".to_string()];
        if let Some(interfaces_str) = self.interfaces.as_ref() {
            vec.append(
                &mut interfaces_str
                    .split(',')
                    .map(|chunk| chunk.trim().to_string())
                    .collect(),
            );
        }
        vec
    }
}

fn get_type_struct_interface_impl(
    interface_name: &str,
    first_field_name: &Ident,
    type_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "TypeInterface" => {
            quote! {
                impl crate::TypeInterface for #type_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::Type> {
                        self.#first_field_name.arena_id()
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::Type>) {
                        self.#first_field_name.set_arena_id(id)
                    }

                    fn flags(&self) -> crate::TypeFlags {
                        self.#first_field_name.flags()
                    }

                    fn set_flags(&self, flags: crate::TypeFlags) {
                        self.#first_field_name.set_flags(flags)
                    }

                    fn id(&self) -> crate::TypeId {
                        self.#first_field_name.id()
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_symbol()
                    }

                    fn symbol(&self) -> ::id_arena::Id<crate::Symbol> {
                        self.#first_field_name.symbol()
                    }

                    fn set_symbol(&self, symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_symbol(symbol)
                    }

                    fn maybe_pattern(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_pattern()
                    }

                    fn set_pattern(&self, pattern: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_pattern(pattern);
                    }

                    fn maybe_alias_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_alias_symbol()
                    }

                    fn set_alias_symbol(&self, alias_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_alias_symbol(alias_symbol)
                    }

                    fn maybe_alias_type_arguments(&self) -> ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>> {
                        self.#first_field_name.maybe_alias_type_arguments()
                    }

                    fn set_alias_type_arguments(&self, alias_type_arguments: ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>) {
                        self.#first_field_name.set_alias_type_arguments(alias_type_arguments);
                    }

                    fn maybe_alias_type_arguments_contains_marker(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_alias_type_arguments_contains_marker()
                    }

                    fn set_alias_type_arguments_contains_marker(
                        &self,
                        alias_type_arguments_contains_marker: ::std::option::Option<bool>,
                    ) {
                        self.#first_field_name.set_alias_type_arguments_contains_marker(alias_type_arguments_contains_marker);
                    }

                    fn maybe_immediate_base_constraint(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_immediate_base_constraint()
                    }

                    fn set_immediate_base_constraint(&self, immediate_base_constraint: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_immediate_base_constraint(immediate_base_constraint)
                    }

                    fn maybe_widened(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_widened()
                    }

                    fn set_widened(&self, widened: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_widened(widened)
                    }

                    fn maybe_restrictive_instantiation(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_restrictive_instantiation()
                    }

                    fn set_restrictive_instantiation(&self, restrictive_instantiation: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_restrictive_instantiation(restrictive_instantiation)
                    }

                    fn maybe_permissive_instantiation(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_permissive_instantiation()
                    }

                    fn set_permissive_instantiation(&self, permissive_instantiation: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_permissive_instantiation(permissive_instantiation)
                    }

                    fn maybe_resolved_base_constraint(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_resolved_base_constraint()
                    }

                    fn set_resolved_base_constraint(&self, resolved_base_constraint: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_resolved_base_constraint(resolved_base_constraint)
                    }

                    fn maybe_resolved_index_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_resolved_index_type()
                    }

                    fn set_resolved_index_type(&self, resolved_index_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_resolved_index_type(resolved_index_type)
                    }

                    fn maybe_resolved_string_index_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_resolved_string_index_type()
                    }

                    fn set_resolved_string_index_type(&self, resolved_string_index_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_resolved_string_index_type(resolved_string_index_type)
                    }

                    fn maybe_synthetic_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_synthetic_type()
                    }

                    fn set_synthetic_type(&self, synthetic_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_synthetic_type(synthetic_type)
                    }

                    fn maybe_default_only_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_default_only_type()
                    }

                    fn set_default_only_type(&self, default_only_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_default_only_type(default_only_type);
                    }

                    fn maybe_promise_type_of_promise_constructor(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_promise_type_of_promise_constructor()
                    }

                    fn set_promise_type_of_promise_constructor(&self, promise_type_of_promise_constructor: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_promise_type_of_promise_constructor(promise_type_of_promise_constructor);
                    }

                    fn maybe_promised_type_of_promise(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_promised_type_of_promise()
                    }

                    fn set_promised_type_of_promise(&self, promised_type_of_promise: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_promised_type_of_promise(promised_type_of_promise)
                    }

                    fn maybe_awaited_type_of_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_awaited_type_of_type()
                    }

                    fn set_awaited_type_of_type(&self, awaited_type_of_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_awaited_type_of_type(awaited_type_of_type)
                    }

                    fn maybe_iteration_types_of_generator_return_type(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_generator_return_type()
                    }

                    fn set_iteration_types_of_generator_return_type(&self, iteration_types_of_generator_return_type: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_generator_return_type(iteration_types_of_generator_return_type)
                    }

                    fn maybe_iteration_types_of_async_generator_return_type(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_async_generator_return_type()
                    }

                    fn set_iteration_types_of_async_generator_return_type(&self, iteration_types_of_async_generator_return_type: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_async_generator_return_type(iteration_types_of_async_generator_return_type)
                    }

                    fn maybe_iteration_types_of_iterable(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_iterable()
                    }

                    fn set_iteration_types_of_iterable(&self, iteration_types_of_iterable: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_iterable(iteration_types_of_iterable)
                    }

                    fn maybe_iteration_types_of_iterator(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_iterator()
                    }

                    fn set_iteration_types_of_iterator(&self, iteration_types_of_iterator: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_iterator(iteration_types_of_iterator)
                    }

                    fn maybe_iteration_types_of_async_iterable(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_async_iterable()
                    }

                    fn set_iteration_types_of_async_iterable(&self, iteration_types_of_async_iterable: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_async_iterable(iteration_types_of_async_iterable)
                    }

                    fn maybe_iteration_types_of_async_iterator(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_async_iterator()
                    }

                    fn set_iteration_types_of_async_iterator(&self, iteration_types_of_async_iterator: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_async_iterator(iteration_types_of_async_iterator)
                    }

                    fn maybe_iteration_types_of_iterator_result(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.maybe_iteration_types_of_iterator_result()
                    }

                    fn set_iteration_types_of_iterator_result(&self, iteration_types_of_iterator_result: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        self.#first_field_name.set_iteration_types_of_iterator_result(iteration_types_of_iterator_result)
                    }

                    fn get_by_iteration_type_cache_key(
                        &self,
                        key: crate::IterationTypeCacheKey,
                    ) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        self.#first_field_name.get_by_iteration_type_cache_key(key)
                    }

                    fn set_by_iteration_type_cache_key(
                        &self,
                        key: crate::IterationTypeCacheKey,
                        value: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>,
                    ) {
                        self.#first_field_name.set_by_iteration_type_cache_key(key, value)
                    }
                }
            }
        }
        "IntrinsicTypeInterface" => {
            quote! {
                impl crate::IntrinsicTypeInterface for #type_type_name {
                    fn intrinsic_name(&self) -> &str {
                        self.#first_field_name.intrinsic_name()
                    }
                }
            }
        }
        "LiteralTypeInterface" => {
            quote! {
                impl crate::LiteralTypeInterface for #type_type_name {
                    fn fresh_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.fresh_type()
                    }

                    fn set_fresh_type(&self, fresh_type: ::id_arena::Id<crate::Type>) {
                        self.#first_field_name.set_fresh_type(fresh_type)
                    }

                    fn regular_type(&self) -> ::id_arena::Id<crate::Type> {
                        self.#first_field_name.regular_type()
                    }

                    fn set_regular_type(&self, regular_type: ::id_arena::Id<crate::Type>) {
                        self.#first_field_name.set_regular_type(regular_type)
                    }
                }
            }
        }
        "ObjectFlagsTypeInterface" => {
            quote! {
                impl crate::ObjectFlagsTypeInterface for #type_type_name {
                    fn object_flags(&self) -> crate::ObjectFlags {
                        self.#first_field_name.object_flags()
                    }

                    fn set_object_flags(&self, object_flags: crate::ObjectFlags) {
                        self.#first_field_name.set_object_flags(object_flags)
                    }
                }
            }
        }
        "ObjectTypeInterface" => {
            quote! {
                impl crate::ObjectTypeInterface for #type_type_name {
                    fn maybe_members(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.maybe_members()
                    }

                    fn set_members(&self, members: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        self.#first_field_name.set_members(members)
                    }

                    fn maybe_properties(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        self.#first_field_name.maybe_properties()
                    }

                    fn maybe_call_signatures(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Signature>>>> {
                        self.#first_field_name.maybe_call_signatures()
                    }

                    fn maybe_target(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_target()
                    }

                    fn maybe_mapper(&self) -> ::std::option::Option<::id_arena::Id<crate::TypeMapper>> {
                        self.#first_field_name.maybe_mapper()
                    }

                    fn maybe_instantiations(&self) -> ::std::cell::RefMut<::std::option::Option<::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>>> {
                        self.#first_field_name.maybe_instantiations()
                    }
                }
            }
        }
        "ResolvableTypeInterface" => {
            quote! {
                impl crate::ResolvableTypeInterface for #type_type_name {
                    fn resolve(&self, members: ::id_arena::Id<crate::SymbolTable>, properties: ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>, call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>, construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>, index_infos: ::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>) {
                        self.#first_field_name.resolve(members, properties, call_signatures, construct_signatures, index_infos)
                    }

                    fn is_resolved(&self) -> bool {
                        self.#first_field_name.is_resolved()
                    }
                }
            }
        }
        "ResolvedTypeInterface" => {
            quote! {
                impl crate::ResolvedTypeInterface for #type_type_name {
                    fn members(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        self.#first_field_name.members()
                    }

                    fn properties(&self) -> ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>> {
                        self.#first_field_name.properties()
                    }

                    fn set_properties(&self, properties: ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>) {
                        self.#first_field_name.set_properties(properties)
                    }

                    fn call_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        self.#first_field_name.call_signatures()
                    }

                    fn set_call_signatures(&self, call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        self.#first_field_name.set_call_signatures(call_signatures)
                    }

                    fn construct_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        self.#first_field_name.construct_signatures()
                    }

                    fn set_construct_signatures(&self, construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        self.#first_field_name.set_construct_signatures(construct_signatures)
                    }

                    fn index_infos(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>> {
                        self.#first_field_name.index_infos()
                    }

                    fn maybe_object_type_without_abstract_construct_signatures(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_object_type_without_abstract_construct_signatures()
                    }

                    fn set_object_type_without_abstract_construct_signatures(
                        &self,
                        object_type_without_abstract_construct_signatures: ::std::option::Option<::id_arena::Id<crate::Type>>,
                    ) {
                        self.#first_field_name.set_object_type_without_abstract_construct_signatures(object_type_without_abstract_construct_signatures)
                    }
                }
            }
        }
        "FreshObjectLiteralTypeInterface" => {
            quote! {
                impl crate::FreshObjectLiteralTypeInterface for #type_type_name {
                    fn maybe_regular_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_regular_type()
                    }

                    fn set_regular_type(&self, regular_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_regular_type(regular_type)
                    }
                }
            }
        }
        "UnionOrIntersectionTypeInterface" => {
            quote! {
                impl crate::UnionOrIntersectionTypeInterface for #type_type_name {
                    fn types(&self) -> &[::id_arena::Id<crate::Type>] {
                        self.#first_field_name.types()
                    }

                    fn maybe_property_cache(&self) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        self.#first_field_name.maybe_property_cache()
                    }

                    fn maybe_property_cache_without_object_function_property_augment(
                        &self,
                    ) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        self.#first_field_name.maybe_property_cache_without_object_function_property_augment()
                    }

                    fn maybe_resolved_properties(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        self.#first_field_name.maybe_resolved_properties()
                    }

                    fn set_resolved_properties(&self, resolved_properties: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>>) {
                        self.#first_field_name.set_resolved_properties(resolved_properties)
                    }
                }
            }
        }
        "InterfaceTypeWithDeclaredMembersInterface" => {
            quote! {
                impl crate::InterfaceTypeWithDeclaredMembersInterface for #type_type_name {
                    fn maybe_declared_properties(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        self.#first_field_name.maybe_declared_properties()
                    }

                    fn set_declared_properties(&self, declared_properties: ::std::vec::Vec<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_declared_properties(declared_properties)
                    }

                    fn declared_call_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        self.#first_field_name.declared_call_signatures()
                    }

                    fn set_declared_call_signatures(&self, declared_call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        self.#first_field_name.set_declared_call_signatures(declared_call_signatures)
                    }

                    fn declared_construct_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        self.#first_field_name.declared_construct_signatures()
                    }

                    fn set_declared_construct_signatures(&self, declared_construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        self.#first_field_name.set_declared_construct_signatures(declared_construct_signatures)
                    }

                    fn declared_index_infos(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>> {
                        self.#first_field_name.declared_index_infos()
                    }

                    fn set_declared_index_infos(&self, declared_index_infos: ::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>) {
                        self.#first_field_name.set_declared_index_infos(declared_index_infos)
                    }
                }
            }
        }
        "GenericableTypeInterface" => {
            quote! {
                impl crate::GenericableTypeInterface for #type_type_name {
                    fn genericize(&self, instantiations: ::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.genericize(instantiations)
                    }
                }
            }
        }
        "GenericTypeInterface" => {
            quote! {
                impl crate::GenericTypeInterface for #type_type_name {
                    fn instantiations(&self) -> ::std::cell::RefMut<::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>> {
                        self.#first_field_name.instantiations()
                    }

                    fn maybe_variances(&self) -> ::std::rc::Rc<::std::cell::RefCell<::std::option::Option<::std::vec::Vec<crate::VarianceFlags>>>> {
                        self.#first_field_name.maybe_variances()
                    }

                    fn set_variances(&self, variances: ::std::vec::Vec<crate::VarianceFlags>) {
                        self.#first_field_name.set_variances(variances)
                    }
                }
            }
        }
        "InterfaceTypeInterface" => {
            quote! {
                impl crate::InterfaceTypeInterface for #type_type_name {
                    fn maybe_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        self.#first_field_name.maybe_type_parameters()
                    }

                    fn maybe_outer_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        self.#first_field_name.maybe_outer_type_parameters()
                    }

                    fn maybe_local_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        self.#first_field_name.maybe_local_type_parameters()
                    }

                    fn maybe_this_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_this_type()
                    }

                    fn maybe_resolved_base_constructor_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_resolved_base_constructor_type()
                    }

                    fn set_resolved_base_constructor_type(&self, resolved_base_constructor_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_resolved_base_constructor_type(resolved_base_constructor_type)
                    }

                    fn maybe_resolved_base_types(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        self.#first_field_name.maybe_resolved_base_types()
                    }

                    fn set_resolved_base_types(&self, resolved_base_types: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Type>>>>) {
                        self.#first_field_name.set_resolved_base_types(resolved_base_types)
                    }

                    fn maybe_base_types_resolved(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_base_types_resolved()
                    }

                    fn set_base_types_resolved(&self, base_types_resolved: ::std::option::Option<bool>) {
                        self.#first_field_name.set_base_types_resolved(base_types_resolved)
                    }
                }
            }
        }
        "TypeReferenceInterface" => {
            quote! {
                impl crate::TypeReferenceInterface for #type_type_name {
                    fn target(&self) -> ::id_arena::Id<crate::Type> {
                        self.#first_field_name.target()
                    }

                    fn set_target(&self, target: ::id_arena::Id<crate::Type>) {
                        self.#first_field_name.set_target(target)
                    }

                    fn maybe_node(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_node()
                    }

                    fn set_node(&self, node: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_node(node)
                    }

                    fn maybe_resolved_type_arguments(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        self.#first_field_name.maybe_resolved_type_arguments()
                    }

                    fn maybe_resolved_type_arguments_mut(&self) -> ::std::cell::RefMut<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        self.#first_field_name.maybe_resolved_type_arguments_mut()
                    }

                    fn maybe_literal_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_literal_type()
                    }

                    fn set_literal_type(&self, literal_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_literal_type(literal_type)
                    }

                    fn maybe_cached_equivalent_base_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        self.#first_field_name.maybe_cached_equivalent_base_type()
                    }

                    fn set_cached_equivalent_base_type(&self, cached_equivalent_base_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        self.#first_field_name.set_cached_equivalent_base_type(cached_equivalent_base_type)
                    }
                }
            }
        }

        _ => panic!("Unknown interface: {}", interface_name),
    }
}

fn get_type_enum_interface_impl(
    interface_name: &str,
    variant_names: &[&Ident],
    type_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "TypeInterface" => {
            quote! {
                impl crate::TypeInterface for #type_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.arena_id()),*
                        }
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_arena_id(id)),*
                        }
                    }

                    fn flags(&self) -> crate::TypeFlags {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.flags()),*
                        }
                    }

                    fn set_flags(&self, flags: crate::TypeFlags) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_flags(flags)),*
                        }
                    }

                    fn id(&self) -> crate::TypeId {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.id()),*
                        }
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_symbol()),*
                        }
                    }

                    fn symbol(&self) -> ::id_arena::Id<crate::Symbol> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.symbol()),*
                        }
                    }

                    fn set_symbol(&self, symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_symbol(symbol)),*
                        }
                    }

                    fn maybe_pattern(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_pattern()),*
                        }
                    }

                    fn set_pattern(&self, pattern: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_pattern(pattern)),*
                        }
                    }

                    fn maybe_alias_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_alias_symbol()),*
                        }
                    }

                    fn set_alias_symbol(&self, alias_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_alias_symbol(alias_symbol)),*
                        }
                    }

                    fn maybe_alias_type_arguments(&self) -> ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_alias_type_arguments()),*
                        }
                    }

                    fn set_alias_type_arguments(&self, alias_type_arguments: ::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_alias_type_arguments(alias_type_arguments)),*
                        }
                    }

                    fn maybe_alias_type_arguments_contains_marker(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_alias_type_arguments_contains_marker()),*
                        }
                    }

                    fn set_alias_type_arguments_contains_marker(
                        &self,
                        alias_type_arguments_contains_marker: ::std::option::Option<bool>,
                    ) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_alias_type_arguments_contains_marker(alias_type_arguments_contains_marker)),*
                        }
                    }

                    fn maybe_immediate_base_constraint(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_immediate_base_constraint()),*
                        }
                    }

                    fn set_immediate_base_constraint(&self, immediate_base_constraint: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_immediate_base_constraint(immediate_base_constraint)),*
                        }
                    }

                    fn maybe_widened(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_widened()),*
                        }
                    }

                    fn set_widened(&self, widened: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_widened(widened)),*
                        }
                    }

                    fn maybe_restrictive_instantiation(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_restrictive_instantiation()),*
                        }
                    }

                    fn set_restrictive_instantiation(&self, restrictive_instantiation: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_restrictive_instantiation(restrictive_instantiation)),*
                        }
                    }

                    fn maybe_permissive_instantiation(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_permissive_instantiation()),*
                        }
                    }

                    fn set_permissive_instantiation(&self, permissive_instantiation: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_permissive_instantiation(permissive_instantiation)),*
                        }
                    }

                    fn maybe_resolved_base_constraint(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_base_constraint()),*
                        }
                    }

                    fn set_resolved_base_constraint(&self, resolved_base_constraint: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_base_constraint(resolved_base_constraint)),*
                        }
                    }

                    fn maybe_resolved_index_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_index_type()),*
                        }
                    }

                    fn set_resolved_index_type(&self, resolved_index_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_index_type(resolved_index_type)),*
                        }
                    }

                    fn maybe_resolved_string_index_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_string_index_type()),*
                        }
                    }

                    fn set_resolved_string_index_type(&self, resolved_string_index_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_string_index_type(resolved_string_index_type)),*
                        }
                    }

                    fn maybe_synthetic_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_synthetic_type()),*
                        }
                    }

                    fn set_synthetic_type(&self, synthetic_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_synthetic_type(synthetic_type)),*
                        }
                    }

                    fn maybe_default_only_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_default_only_type()),*
                        }
                    }

                    fn set_default_only_type(&self, default_only_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_default_only_type(default_only_type)),*
                        }
                    }

                    fn maybe_promise_type_of_promise_constructor(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_promise_type_of_promise_constructor()),*
                        }
                    }

                    fn set_promise_type_of_promise_constructor(&self, promise_type_of_promise_constructor: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_promise_type_of_promise_constructor(promise_type_of_promise_constructor)),*
                        }
                    }

                    fn maybe_promised_type_of_promise(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_promised_type_of_promise()),*
                        }
                    }

                    fn set_promised_type_of_promise(&self, promised_type_of_promise: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_promised_type_of_promise(promised_type_of_promise)),*
                        }
                    }

                    fn maybe_awaited_type_of_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_awaited_type_of_type()),*
                        }
                    }

                    fn set_awaited_type_of_type(&self, awaited_type_of_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_awaited_type_of_type(awaited_type_of_type)),*
                        }
                    }

                    fn maybe_iteration_types_of_generator_return_type(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_generator_return_type()),*
                        }
                    }

                    fn set_iteration_types_of_generator_return_type(&self, iteration_types_of_generator_return_type: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_generator_return_type(iteration_types_of_generator_return_type)),*
                        }
                    }

                    fn maybe_iteration_types_of_async_generator_return_type(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_async_generator_return_type()),*
                        }
                    }

                    fn set_iteration_types_of_async_generator_return_type(&self, iteration_types_of_async_generator_return_type: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_async_generator_return_type(iteration_types_of_async_generator_return_type)),*
                        }
                    }

                    fn maybe_iteration_types_of_iterable(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_iterable()),*
                        }
                    }

                    fn set_iteration_types_of_iterable(&self, iteration_types_of_iterable: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_iterable(iteration_types_of_iterable)),*
                        }
                    }

                    fn maybe_iteration_types_of_iterator(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_iterator()),*
                        }
                    }

                    fn set_iteration_types_of_iterator(&self, iteration_types_of_iterator: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_iterator(iteration_types_of_iterator)),*
                        }
                    }

                    fn maybe_iteration_types_of_async_iterable(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_async_iterable()),*
                        }
                    }

                    fn set_iteration_types_of_async_iterable(&self, iteration_types_of_async_iterable: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_async_iterable(iteration_types_of_async_iterable)),*
                        }
                    }

                    fn maybe_iteration_types_of_async_iterator(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_async_iterator()),*
                        }
                    }

                    fn set_iteration_types_of_async_iterator(&self, iteration_types_of_async_iterator: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_async_iterator(iteration_types_of_async_iterator)),*
                        }
                    }

                    fn maybe_iteration_types_of_iterator_result(&self) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_iteration_types_of_iterator_result()),*
                        }
                    }

                    fn set_iteration_types_of_iterator_result(&self, iteration_types_of_iterator_result: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_iteration_types_of_iterator_result(iteration_types_of_iterator_result)),*
                        }
                    }

                    fn get_by_iteration_type_cache_key(
                        &self,
                        key: crate::IterationTypeCacheKey,
                    ) -> ::std::option::Option<::id_arena::Id<crate::IterationTypes>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.get_by_iteration_type_cache_key(key)),*
                        }
                    }

                    fn set_by_iteration_type_cache_key(
                        &self,
                        key: crate::IterationTypeCacheKey,
                        value: ::std::option::Option<::id_arena::Id<crate::IterationTypes>>,
                    ) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_by_iteration_type_cache_key(key, value)),*
                        }
                    }
                }
            }
        }
        "IntrinsicTypeInterface" => {
            quote! {
                impl crate::IntrinsicTypeInterface for #type_type_name {
                    fn intrinsic_name(&self) -> &str {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.intrinsic_name()),*
                        }
                    }
                }
            }
        }
        "LiteralTypeInterface" => {
            quote! {
                impl crate::LiteralTypeInterface for #type_type_name {
                    fn fresh_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.fresh_type()),*
                        }
                    }

                    fn set_fresh_type(&self, fresh_type: ::id_arena::Id<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_fresh_type(fresh_type)),*
                        }
                    }

                    fn regular_type(&self) -> ::id_arena::Id<crate::Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.regular_type()),*
                        }
                    }

                    fn set_regular_type(&self, regular_type: ::id_arena::Id<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_regular_type(regular_type)),*
                        }
                    }
                }
            }
        }
        "ObjectFlagsTypeInterface" => {
            quote! {
                impl crate::ObjectFlagsTypeInterface for #type_type_name {
                    fn object_flags(&self) -> crate::ObjectFlags {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.object_flags()),*
                        }
                    }

                    fn set_object_flags(&self, object_flags: crate::ObjectFlags) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_object_flags(object_flags)),*
                        }
                    }
                }
            }
        }
        "ObjectTypeInterface" => {
            quote! {
                impl crate::ObjectTypeInterface for #type_type_name {
                    fn maybe_members(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_members()),*
                        }
                    }

                    fn set_members(&self, members: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_members(members)),*
                        }
                    }

                    fn maybe_properties(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_properties()),*
                        }
                    }

                    fn maybe_call_signatures(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Signature>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_call_signatures()),*
                        }
                    }

                    fn maybe_target(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_target()),*
                        }
                    }

                    fn maybe_mapper(&self) -> ::std::option::Option<::id_arena::Id<crate::TypeMapper>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_mapper()),*
                        }
                    }

                    fn maybe_instantiations(&self) -> ::std::cell::RefMut<::std::option::Option<::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_instantiations()),*
                        }
                    }
                }
            }
        }
        "ResolvableTypeInterface" => {
            quote! {
                impl crate::ResolvableTypeInterface for #type_type_name {
                    fn resolve(&self, members: ::id_arena::Id<crate::SymbolTable>, properties: ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>, call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>, construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>, index_infos: ::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.resolve(members, properties, call_signatures, construct_signatures, index_infos)),*
                        }
                    }

                    fn is_resolved(&self) -> bool {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.is_resolved()),*
                        }
                    }
                }
            }
        }
        "ResolvedTypeInterface" => {
            quote! {
                impl crate::ResolvedTypeInterface for #type_type_name {
                    fn members(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.members()),*
                        }
                    }

                    fn properties(&self) -> ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.properties()),*
                        }
                    }

                    fn set_properties(&self, properties: ::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_properties(properties)),*
                        }
                    }

                    fn call_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.call_signatures()),*
                        }
                    }

                    fn set_call_signatures(&self, call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_call_signatures(call_signatures)),*
                        }
                    }

                    fn construct_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.construct_signatures()),*
                        }
                    }

                    fn set_construct_signatures(&self, construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_construct_signatures(construct_signatures)),*
                        }
                    }

                    fn index_infos(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.index_infos()),*
                        }
                    }

                    fn maybe_object_type_without_abstract_construct_signatures(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_object_type_without_abstract_construct_signatures()),*
                        }
                    }

                    fn set_object_type_without_abstract_construct_signatures(
                        &self,
                        object_type_without_abstract_construct_signatures: ::std::option::Option<::id_arena::Id<crate::Type>>,
                    ) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_object_type_without_abstract_construct_signatures(object_type_without_abstract_construct_signatures)),*
                        }
                    }
                }
            }
        }
        "FreshObjectLiteralTypeInterface" => {
            quote! {
                impl crate::FreshObjectLiteralTypeInterface for #type_type_name {
                    fn maybe_regular_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_regular_type()),*
                        }
                    }

                    fn set_regular_type(&self, regular_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_regular_type(regular_type)),*
                        }
                    }
                }
            }
        }
        "UnionOrIntersectionTypeInterface" => {
            quote! {
                impl crate::UnionOrIntersectionTypeInterface for #type_type_name {
                    fn types(&self) -> &[::id_arena::Id<crate::Type>] {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.types()),*
                        }
                    }

                    fn maybe_property_cache(&self) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_property_cache()),*
                        }
                    }

                    fn maybe_property_cache_without_object_function_property_augment(
                        &self,
                    ) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_property_cache_without_object_function_property_augment()),*
                        }
                    }

                    fn maybe_resolved_properties(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_properties()),*
                        }
                    }

                    fn set_resolved_properties(&self, resolved_properties: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_properties(resolved_properties)),*
                        }
                    }
                }
            }
        }
        "InterfaceTypeWithDeclaredMembersInterface" => {
            quote! {
                impl crate::InterfaceTypeWithDeclaredMembersInterface for #type_type_name {
                    fn maybe_declared_properties(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Symbol>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_declared_properties()),*
                        }
                    }

                    fn set_declared_properties(&self, declared_properties: ::std::vec::Vec<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_declared_properties(declared_properties)),*
                        }
                    }

                    fn declared_call_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.declared_call_signatures()),*
                        }
                    }

                    fn set_declared_call_signatures(&self, declared_call_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_declared_call_signatures(declared_call_signatures)),*
                        }
                    }

                    fn declared_construct_signatures(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::Signature>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.declared_construct_signatures()),*
                        }
                    }

                    fn set_declared_construct_signatures(&self, declared_construct_signatures: ::std::vec::Vec<::id_arena::Id<crate::Signature>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_declared_construct_signatures(declared_construct_signatures)),*
                        }
                    }

                    fn declared_index_infos(&self) -> ::std::cell::Ref<::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.declared_index_infos()),*
                        }
                    }

                    fn set_declared_index_infos(&self, declared_index_infos: ::std::vec::Vec<::id_arena::Id<crate::IndexInfo>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_declared_index_infos(declared_index_infos)),*
                        }
                    }
                }
            }
        }
        "GenericableTypeInterface" => {
            quote! {
                impl crate::GenericableTypeInterface for #type_type_name {
                    fn genericize(&self, instantiations: ::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.genericize(instantiations)),*
                        }
                    }
                }
            }
        }
        "GenericTypeInterface" => {
            quote! {
                impl crate::GenericTypeInterface for #type_type_name {
                    fn instantiations(&self) -> ::std::cell::RefMut<::std::collections::HashMap<String, ::id_arena::Id<crate::Type>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.instantiations()),*
                        }
                    }

                    fn maybe_variances(&self) -> ::std::rc::Rc<::std::cell::RefCell<::std::option::Option<::std::vec::Vec<crate::VarianceFlags>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_variances()),*
                        }
                    }

                    fn set_variances(&self, variances: ::std::vec::Vec<crate::VarianceFlags>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_variances(variances)),*
                        }
                    }
                }
            }
        }
        "InterfaceTypeInterface" => {
            quote! {
                impl crate::InterfaceTypeInterface for #type_type_name {
                    fn maybe_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_type_parameters()),*
                        }
                    }

                    fn maybe_outer_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_outer_type_parameters()),*
                        }
                    }

                    fn maybe_local_type_parameters(&self) -> ::std::option::Option<&[::id_arena::Id<crate::Type>]> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_local_type_parameters()),*
                        }
                    }

                    fn maybe_this_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_this_type()),*
                        }
                    }

                    fn maybe_resolved_base_constructor_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_base_constructor_type()),*
                        }
                    }

                    fn set_resolved_base_constructor_type(&self, resolved_base_constructor_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_base_constructor_type(resolved_base_constructor_type)),*
                        }
                    }

                    fn maybe_resolved_base_types(&self) -> ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_base_types()),*
                        }
                    }

                    fn set_resolved_base_types(&self, resolved_base_types: ::std::option::Option<::id_arena::Id<::std::vec::Vec<::id_arena::Id<crate::Type>>>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_resolved_base_types(resolved_base_types)),*
                        }
                    }

                    fn maybe_base_types_resolved(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_base_types_resolved()),*
                        }
                    }

                    fn set_base_types_resolved(&self, base_types_resolved: ::std::option::Option<bool>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_base_types_resolved(base_types_resolved)),*
                        }
                    }
                }
            }
        }
        "TypeReferenceInterface" => {
            quote! {
                impl crate::TypeReferenceInterface for #type_type_name {
                    fn target(&self) -> ::id_arena::Id<crate::Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.target()),*
                        }
                    }

                    fn set_target(&self, target: ::id_arena::Id<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_target(target)),*
                        }
                    }

                    fn maybe_node(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_node()),*
                        }
                    }

                    fn set_node(&self, node: ::std::option::Option<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_node(node)),*
                        }
                    }

                    fn maybe_resolved_type_arguments(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_type_arguments()),*
                        }
                    }

                    fn maybe_resolved_type_arguments_mut(&self) -> ::std::cell::RefMut<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Type>>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_resolved_type_arguments_mut()),*
                        }
                    }

                    fn maybe_literal_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_literal_type()),*
                        }
                    }

                    fn set_literal_type(&self, literal_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_literal_type(literal_type)),*
                        }
                    }

                    fn maybe_cached_equivalent_base_type(&self) -> ::std::option::Option<::id_arena::Id<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_cached_equivalent_base_type()),*
                        }
                    }

                    fn set_cached_equivalent_base_type(&self, cached_equivalent_base_type: ::std::option::Option<::id_arena::Id<crate::Type>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_cached_equivalent_base_type(cached_equivalent_base_type)),*
                        }
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

#[proc_macro_attribute]
pub fn type_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: type_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let args = match TypeTypeArgs::from_list(&attr_args) {
        Ok(args) => args,
        Err(error) => {
            return TokenStream::from(error.write_errors());
        }
    };

    let type_interface_implementation = match data {
        Struct(struct_) => {
            let first_field_name = match struct_.fields {
                Fields::Named(FieldsNamed { named, .. }) => named
                    .iter()
                    .nth(0)
                    .expect("Expected at least one struct field")
                    .ident
                    .clone()
                    .expect("Expected ident"),
                _ => panic!("Expected named fields"),
            };

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl =
                    get_type_struct_interface_impl(&interface, &first_field_name, &type_type_name);
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        Enum(DataEnum { variants, .. }) => {
            let variant_names = variants
                .iter()
                .map(|variant| &variant.ident)
                .collect::<Vec<_>>();

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl =
                    get_type_enum_interface_impl(&interface, &variant_names, &type_type_name);
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        _ => panic!("Expected struct or enum"),
    };

    let into_implementations = if args.should_impl_from() {
        let mut construct_variant = quote! {
            concrete
        };
        let mut previous_variant_name = type_type_name.clone();
        let mut into_implementations = quote! {};
        for ancestor in args.ancestors_vec() {
            let ancestor_ident = Ident::new(&ancestor, previous_variant_name.span());
            construct_variant = quote! {
                crate::#ancestor_ident::#previous_variant_name(#construct_variant)
            };
            into_implementations = quote! {
                #into_implementations

                impl ::std::convert::From<#type_type_name> for crate::#ancestor_ident {
                    fn from(concrete: #type_type_name) -> Self {
                        #construct_variant
                    }
                }
            };
            previous_variant_name = ancestor_ident;
        }

        quote! {
            #into_implementations
        }
    } else {
        quote! {}
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #type_interface_implementation

        #into_implementations
    }
    .into()
}

#[derive(Debug, FromMeta)]
struct SymbolTypeArgs {
    #[darling(default)]
    ancestors: Option<String>,
    #[darling(default)]
    impl_from: Option<bool>,
    #[darling(default)]
    interfaces: Option<String>,
}

impl SymbolTypeArgs {
    fn ancestors_vec(&self) -> Vec<String> {
        let mut vec = self.ancestors.as_ref().map_or_else(
            || vec![],
            |ancestors_str| {
                ancestors_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect()
            },
        );
        vec.push("Symbol".to_string());
        vec
    }

    fn should_impl_from(&self) -> bool {
        self.impl_from.unwrap_or(true)
    }

    fn interfaces_vec(&self) -> Vec<String> {
        let mut vec = vec!["SymbolInterface".to_string()];
        if let Some(interfaces_str) = self.interfaces.as_ref() {
            vec.append(
                &mut interfaces_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect(),
            );
        }
        vec
    }
}

fn get_symbol_struct_interface_impl(
    interface_name: &str,
    first_field_name: &Ident,
    symbol_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "SymbolInterface" => {
            quote! {
                impl crate::SymbolInterface for #symbol_type_name {
                    fn flags(&self) -> crate::SymbolFlags {
                        self.#first_field_name.flags()
                    }

                    fn set_flags(&self, flags: crate::SymbolFlags) {
                        self.#first_field_name.set_flags(flags)
                    }

                    fn escaped_name(&self) -> &str {
                        self.#first_field_name.escaped_name()
                    }

                    fn maybe_declarations(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        self.#first_field_name.maybe_declarations()
                    }

                    fn maybe_declarations_mut(&self) -> ::std::cell::RefMut<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        self.#first_field_name.maybe_declarations_mut()
                    }

                    fn set_declarations(&self, declarations: ::std::vec::Vec<::id_arena::Id<crate::Node>>) {
                        self.#first_field_name.set_declarations(declarations)
                    }

                    fn maybe_value_declaration(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        self.#first_field_name.maybe_value_declaration()
                    }

                    fn set_value_declaration(&self, node: ::id_arena::Id<crate::Node>) {
                        self.#first_field_name.set_value_declaration(node)
                    }

                    fn maybe_members(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.maybe_members()
                    }

                    fn set_members(&self, members: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        self.#first_field_name.set_members(members)
                    }

                    fn members(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        self.#first_field_name.members()
                    }

                    fn maybe_exports(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.maybe_exports()
                    }

                    fn set_exports(&self, exports: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        self.#first_field_name.set_exports(exports)
                    }

                    fn exports(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        self.#first_field_name.exports()
                    }

                    fn maybe_global_exports(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        self.#first_field_name.maybe_global_exports()
                    }

                    fn set_global_exports(&self, global_exports: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        self.#first_field_name.set_global_exports(global_exports)
                    }

                    fn maybe_id(&self) -> ::std::option::Option<crate::SymbolId> {
                        self.#first_field_name.maybe_id()
                    }

                    fn id(&self) -> crate::SymbolId {
                        self.#first_field_name.id()
                    }

                    fn set_id(&self, id: crate::SymbolId) {
                        self.#first_field_name.set_id(id)
                    }

                    fn maybe_merge_id(&self) -> ::std::option::Option<u32> {
                        self.#first_field_name.maybe_merge_id()
                    }

                    fn set_merge_id(&self, merge_id: u32) {
                        self.#first_field_name.set_merge_id(merge_id)
                    }

                    fn maybe_parent(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_parent()
                    }

                    fn set_parent(&self, parent: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_parent(parent)
                    }

                    fn maybe_export_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        self.#first_field_name.maybe_export_symbol()
                    }

                    fn set_export_symbol(&self, export_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        self.#first_field_name.set_export_symbol(export_symbol)
                    }

                    fn maybe_const_enum_only_module(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_const_enum_only_module()
                    }

                    fn set_const_enum_only_module(&self, const_enum_only_module: ::std::option::Option<bool>) {
                        self.#first_field_name.set_const_enum_only_module(const_enum_only_module)
                    }

                    fn maybe_is_referenced(&self) -> ::std::option::Option<crate::SymbolFlags> {
                        self.#first_field_name.maybe_is_referenced()
                    }

                    fn set_is_referenced(&self, is_referenced: ::std::option::Option<crate::SymbolFlags>) {
                        self.#first_field_name.set_is_referenced(is_referenced)
                    }

                    fn maybe_is_replaceable_by_method(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_is_replaceable_by_method()
                    }

                    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: ::std::option::Option<bool>) {
                        self.#first_field_name.set_is_replaceable_by_method(is_replaceable_by_method)
                    }

                    fn maybe_is_assigned(&self) -> ::std::option::Option<bool> {
                        self.#first_field_name.maybe_is_assigned()
                    }

                    fn set_is_assigned(&self, is_assigned: ::std::option::Option<bool>) {
                        self.#first_field_name.set_is_assigned(is_assigned)
                    }

                    fn maybe_assignment_declaration_members(&self) -> ::std::cell::RefMut<::std::option::Option<::std::collections::HashMap<crate::NodeId, ::id_arena::Id<crate::Node>>>> {
                        self.#first_field_name.maybe_assignment_declaration_members()
                    }
                }
            }
        }
        "TransientSymbolInterface" => {
            quote! {
                impl crate::TransientSymbolInterface for #symbol_type_name {
                    fn symbol_links(&self) -> ::id_arena::Id<crate::SymbolLinks> {
                        self.#first_field_name.symbol_links()
                    }

                    fn check_flags(&self) -> crate::CheckFlags {
                        self.#first_field_name.check_flags()
                    }

                    fn set_check_flags(&self, check_flags: crate::CheckFlags) {
                        self.#first_field_name.set_check_flags(check_flags)
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

fn get_symbol_enum_interface_impl(
    interface_name: &str,
    variant_names: &[&Ident],
    symbol_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "SymbolInterface" => {
            quote! {
                impl crate::SymbolInterface for #symbol_type_name {
                    fn flags(&self) -> crate::SymbolFlags {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.flags()),*
                        }
                    }

                    fn set_flags(&self, flags: crate::SymbolFlags) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_flags(flags)),*
                        }
                    }

                    fn escaped_name(&self) -> &str {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.escaped_name()),*
                        }
                    }

                    fn maybe_declarations(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_declarations()),*
                        }
                    }

                    fn maybe_declarations_mut(&self) -> ::std::cell::RefMut<::std::option::Option<::std::vec::Vec<::id_arena::Id<crate::Node>>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_declarations_mut()),*
                        }
                    }

                    fn set_declarations(&self, declarations: ::std::vec::Vec<::id_arena::Id<crate::Node>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_declarations(declarations)),*
                        }
                    }

                    fn maybe_value_declaration(&self) -> ::std::option::Option<::id_arena::Id<crate::Node>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_value_declaration()),*
                        }
                    }

                    fn set_value_declaration(&self, node: ::id_arena::Id<crate::Node>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_value_declaration(node)),*
                        }
                    }

                    fn maybe_members(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_members()),*
                        }
                    }

                    fn set_members(&self, members: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_members(members)),*
                        }
                    }

                    fn members(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.members()),*
                        }
                    }

                    fn maybe_exports(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_exports()),*
                        }
                    }

                    fn set_exports(&self, exports: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_exports(exports)),*
                        }
                    }

                    fn exports(&self) -> ::id_arena::Id<crate::SymbolTable> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.exports()),*
                        }
                    }

                    fn maybe_global_exports(&self) -> ::std::option::Option<::id_arena::Id<crate::SymbolTable>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_global_exports()),*
                        }
                    }

                    fn set_global_exports(&self, global_exports: ::std::option::Option<::id_arena::Id<crate::SymbolTable>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_global_exports(global_exports)),*
                        }
                    }

                    fn maybe_id(&self) -> ::std::option::Option<crate::SymbolId> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_id()),*
                        }
                    }

                    fn id(&self) -> crate::SymbolId {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.id()),*
                        }
                    }

                    fn set_id(&self, id: crate::SymbolId) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_id(id)),*
                        }
                    }

                    fn maybe_merge_id(&self) -> ::std::option::Option<u32> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_merge_id()),*
                        }
                    }

                    fn set_merge_id(&self, merge_id: u32) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_merge_id(merge_id)),*
                        }
                    }

                    fn maybe_parent(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_parent()),*
                        }
                    }

                    fn set_parent(&self, parent: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_parent(parent)),*
                        }
                    }

                    fn maybe_export_symbol(&self) -> ::std::option::Option<::id_arena::Id<crate::Symbol>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_export_symbol()),*
                        }
                    }

                    fn set_export_symbol(&self, export_symbol: ::std::option::Option<::id_arena::Id<crate::Symbol>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_export_symbol(export_symbol)),*
                        }
                    }

                    fn maybe_const_enum_only_module(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_const_enum_only_module()),*
                        }
                    }

                    fn set_const_enum_only_module(&self, const_enum_only_module: ::std::option::Option<bool>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_const_enum_only_module(const_enum_only_module)),*
                        }
                    }

                    fn maybe_is_referenced(&self) -> ::std::option::Option<crate::SymbolFlags> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_is_referenced()),*
                        }
                    }

                    fn set_is_referenced(&self, is_referenced: ::std::option::Option<crate::SymbolFlags>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_is_referenced(is_referenced)),*
                        }
                    }

                    fn maybe_is_replaceable_by_method(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_is_replaceable_by_method()),*
                        }
                    }

                    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: ::std::option::Option<bool>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_is_replaceable_by_method(is_replaceable_by_method)),*
                        }
                    }

                    fn maybe_is_assigned(&self) -> ::std::option::Option<bool> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_is_assigned()),*
                        }
                    }

                    fn set_is_assigned(&self, is_assigned: ::std::option::Option<bool>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_is_assigned(is_assigned)),*
                        }
                    }

                    fn maybe_assignment_declaration_members(&self) -> ::std::cell::RefMut<::std::option::Option<::std::collections::HashMap<crate::NodeId, ::id_arena::Id<crate::Node>>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_assignment_declaration_members()),*
                        }
                    }
                }
            }
        }
        "TransientSymbolInterface" => {
            quote! {
                impl crate::TransientSymbolInterface for #symbol_type_name {
                    fn symbol_links(&self) -> ::id_arena::Id<crate::SymbolLinks> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.symbol_links()),*
                        }
                    }

                    fn check_flags(&self) -> crate::CheckFlags {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.check_flags()),*
                        }
                    }

                    fn set_check_flags(&self, check_flags: crate::CheckFlags) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_check_flags(check_flags)),*
                        }
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

#[proc_macro_attribute]
pub fn symbol_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: symbol_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let args = match SymbolTypeArgs::from_list(&attr_args) {
        Ok(args) => args,
        Err(error) => {
            return TokenStream::from(error.write_errors());
        }
    };

    let symbol_interface_implementation = match data {
        Struct(struct_) => {
            let first_field_name = match struct_.fields {
                Fields::Named(FieldsNamed { named, .. }) => named
                    .iter()
                    .nth(0)
                    .expect("Expected at least one struct field")
                    .ident
                    .clone()
                    .expect("Expected ident"),
                _ => panic!("Expected named fields"),
            };

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl = get_symbol_struct_interface_impl(
                    &interface,
                    &first_field_name,
                    &symbol_type_name,
                );
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        Enum(DataEnum { variants, .. }) => {
            let variant_names = variants
                .iter()
                .map(|variant| &variant.ident)
                .collect::<Vec<_>>();

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl =
                    get_symbol_enum_interface_impl(&interface, &variant_names, &symbol_type_name);
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        _ => panic!("Expected struct or enum"),
    };

    let into_implementations = if args.should_impl_from() {
        let mut construct_variant = quote! {
            concrete
        };
        let mut previous_variant_name = symbol_type_name.clone();
        let mut into_implementations = quote! {};
        for ancestor in args.ancestors_vec() {
            let ancestor_ident = Ident::new(&ancestor, previous_variant_name.span());
            construct_variant = quote! {
                crate::#ancestor_ident::#previous_variant_name(#construct_variant)
            };
            into_implementations = quote! {
                #into_implementations

                impl ::std::convert::From<#symbol_type_name> for crate::#ancestor_ident {
                    fn from(concrete: #symbol_type_name) -> Self {
                        #construct_variant
                    }
                }
            };
            previous_variant_name = ancestor_ident;
        }

        quote! {
            #into_implementations
        }
    } else {
        quote! {}
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #symbol_interface_implementation

        #into_implementations
    }
    .into()
}

#[derive(Debug, FromMeta)]
struct CommandLineOptionTypeArgs {
    #[darling(default)]
    ancestors: Option<String>,
    #[darling(default)]
    impl_from: Option<bool>,
    #[darling(default)]
    interfaces: Option<String>,
}

impl CommandLineOptionTypeArgs {
    fn ancestors_vec(&self) -> Vec<String> {
        let mut vec = self.ancestors.as_ref().map_or_else(
            || vec![],
            |ancestors_str| {
                ancestors_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect()
            },
        );
        vec.push("CommandLineOption".to_string());
        vec
    }

    fn should_impl_from(&self) -> bool {
        self.impl_from.unwrap_or(true)
    }

    fn interfaces_vec(&self) -> Vec<String> {
        let mut vec = vec!["CommandLineOptionInterface".to_string()];
        if let Some(interfaces_str) = self.interfaces.as_ref() {
            vec.append(
                &mut interfaces_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect(),
            );
        }
        vec
    }
}

fn get_command_line_option_struct_interface_impl(
    interface_name: &str,
    first_field_name: &Ident,
    command_line_option_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "CommandLineOptionInterface" => {
            quote! {
                impl crate::CommandLineOptionInterface for #command_line_option_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::CommandLineOption> {
                        self.#first_field_name.arena_id()
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::CommandLineOption>) {
                        self.#first_field_name.set_arena_id(id)
                    }

                    fn name(&self) -> &str {
                        self.#first_field_name.name()
                    }

                    fn type_(&self) -> &crate::CommandLineOptionType {
                        self.#first_field_name.type_()
                    }

                    fn is_file_path(&self) -> bool {
                        self.#first_field_name.is_file_path()
                    }

                    fn maybe_short_name(&self) -> ::std::option::Option<&str> {
                        self.#first_field_name.maybe_short_name()
                    }

                    fn maybe_description(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        self.#first_field_name.maybe_description()
                    }

                    fn maybe_default_value_description(&self) -> ::std::option::Option<&crate::StringOrDiagnosticMessage> {
                        self.#first_field_name.maybe_default_value_description()
                    }

                    fn maybe_param_type(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        self.#first_field_name.maybe_param_type()
                    }

                    fn is_tsconfig_only(&self) -> bool {
                        self.#first_field_name.is_tsconfig_only()
                    }

                    fn is_command_line_only(&self) -> bool {
                        self.#first_field_name.is_command_line_only()
                    }

                    fn show_in_simplified_help_view(&self) -> bool {
                        self.#first_field_name.show_in_simplified_help_view()
                    }

                    fn maybe_category(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        self.#first_field_name.maybe_category()
                    }

                    fn strict_flag(&self) -> bool {
                        self.#first_field_name.strict_flag()
                    }

                    fn affects_source_file(&self) -> bool {
                        self.#first_field_name.affects_source_file()
                    }

                    fn affects_module_resolution(&self) -> bool {
                        self.#first_field_name.affects_module_resolution()
                    }

                    fn affects_bind_diagnostics(&self) -> bool {
                        self.#first_field_name.affects_bind_diagnostics()
                    }

                    fn affects_semantic_diagnostics(&self) -> bool {
                        self.#first_field_name.affects_semantic_diagnostics()
                    }

                    fn affects_emit(&self) -> bool {
                        self.#first_field_name.affects_emit()
                    }

                    fn affects_program_structure(&self) -> bool {
                        self.#first_field_name.affects_program_structure()
                    }

                    fn transpile_option_value(&self) -> ::std::option::Option<::std::option::Option<bool>> {
                        self.#first_field_name.transpile_option_value()
                    }

                    fn maybe_extra_validation(
                        &self,
                    ) -> ::std::option::Option<
                        ::std::rc::Rc<dyn Fn(::std::option::Option<&serde_json::Value>) -> ::std::option::Option<(&'static crate::DiagnosticMessage, ::std::option::Option<Vec<String>>)>>,
                    > {
                        self.#first_field_name.maybe_extra_validation()
                    }

                    fn maybe_extra_validation_compiler_options_value(
                        &self,
                    ) -> ::std::option::Option<
                        ::std::rc::Rc<dyn Fn(&crate::CompilerOptionsValue) -> ::std::option::Option<(&'static crate::DiagnosticMessage, ::std::option::Option<Vec<String>>)>>,
                    > {
                        self.#first_field_name.maybe_extra_validation_compiler_options_value()
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

fn get_command_line_option_enum_interface_impl(
    interface_name: &str,
    variant_names: &[&Ident],
    command_line_option_type_name: &Ident,
) -> TokenStream2 {
    match interface_name {
        "CommandLineOptionInterface" => {
            quote! {
                impl crate::CommandLineOptionInterface for #command_line_option_type_name {
                    fn arena_id(&self) -> ::id_arena::Id<crate::CommandLineOption> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.arena_id()),*
                        }
                    }

                    fn set_arena_id(&self, id: ::id_arena::Id<crate::CommandLineOption>) {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.set_arena_id(id)),*
                        }
                    }

                    fn name(&self) -> &str {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.name()),*
                        }
                    }

                    fn type_(&self) -> &crate::CommandLineOptionType {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.type_()),*
                        }
                    }

                    fn is_file_path(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.is_file_path()),*
                        }
                    }

                    fn maybe_short_name(&self) -> ::std::option::Option<&str> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_short_name()),*
                        }
                    }

                    fn maybe_description(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_description()),*
                        }
                    }

                    fn maybe_default_value_description(&self) -> ::std::option::Option<&crate::StringOrDiagnosticMessage> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_default_value_description()),*
                        }
                    }

                    fn maybe_param_type(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_param_type()),*
                        }
                    }

                    fn is_tsconfig_only(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.is_tsconfig_only()),*
                        }
                    }

                    fn is_command_line_only(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.is_command_line_only()),*
                        }
                    }

                    fn show_in_simplified_help_view(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.show_in_simplified_help_view()),*
                        }
                    }

                    fn maybe_category(&self) -> ::std::option::Option<&crate::DiagnosticMessage> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_category()),*
                        }
                    }

                    fn strict_flag(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.strict_flag()),*
                        }
                    }

                    fn affects_source_file(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_source_file()),*
                        }
                    }

                    fn affects_module_resolution(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_module_resolution()),*
                        }
                    }

                    fn affects_bind_diagnostics(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_bind_diagnostics()),*
                        }
                    }

                    fn affects_semantic_diagnostics(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_semantic_diagnostics()),*
                        }
                    }

                    fn affects_emit(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_emit()),*
                        }
                    }

                    fn affects_program_structure(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.affects_program_structure()),*
                        }
                    }

                    fn transpile_option_value(&self) -> ::std::option::Option<::std::option::Option<bool>> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.transpile_option_value()),*
                        }
                    }

                    fn maybe_extra_validation(
                        &self,
                    ) -> ::std::option::Option<
                        ::std::rc::Rc<dyn Fn(::std::option::Option<&serde_json::Value>) -> ::std::option::Option<(&'static crate::DiagnosticMessage, ::std::option::Option<Vec<String>>)>>,
                    > {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_extra_validation()),*
                        }
                    }

                    fn maybe_extra_validation_compiler_options_value(
                        &self,
                    ) -> ::std::option::Option<
                        ::std::rc::Rc<dyn Fn(&crate::CompilerOptionsValue) -> ::std::option::Option<(&'static crate::DiagnosticMessage, ::std::option::Option<Vec<String>>)>>,
                    > {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.maybe_extra_validation_compiler_options_value()),*
                        }
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

#[proc_macro_attribute]
pub fn command_line_option_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: command_line_option_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let args = match CommandLineOptionTypeArgs::from_list(&attr_args) {
        Ok(args) => args,
        Err(error) => {
            return TokenStream::from(error.write_errors());
        }
    };

    let interfaces_implementation = match data {
        Struct(struct_) => {
            let first_field_name = match struct_.fields {
                Fields::Named(FieldsNamed { named, .. }) => named
                    .iter()
                    .nth(0)
                    .expect("Expected at least one struct field")
                    .ident
                    .clone()
                    .expect("Expected ident"),
                _ => panic!("Expected named fields"),
            };

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl = get_command_line_option_struct_interface_impl(
                    &interface,
                    &first_field_name,
                    &command_line_option_type_name,
                );
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        Enum(DataEnum { variants, .. }) => {
            let variant_names = variants
                .iter()
                .map(|variant| &variant.ident)
                .collect::<Vec<_>>();

            let mut interface_impls: TokenStream2 = quote! {};
            for interface in args.interfaces_vec() {
                let interface_impl = get_command_line_option_enum_interface_impl(
                    &interface,
                    &variant_names,
                    &command_line_option_type_name,
                );
                interface_impls = quote! {
                    #interface_impls

                    #interface_impl
                };
            }

            interface_impls
        }
        _ => panic!("Expected struct or enum"),
    };

    let into_implementations = if args.should_impl_from() {
        let mut construct_variant = quote! {
            concrete
        };
        let mut previous_variant_name = command_line_option_type_name.clone();
        let mut into_implementations = quote! {};
        for ancestor in args.ancestors_vec() {
            let ancestor_ident = Ident::new(&ancestor, previous_variant_name.span());
            construct_variant = quote! {
                crate::#ancestor_ident::#previous_variant_name(#construct_variant)
            };
            into_implementations = quote! {
                #into_implementations

                impl ::std::convert::From<#command_line_option_type_name> for crate::#ancestor_ident {
                    fn from(concrete: #command_line_option_type_name) -> Self {
                        #construct_variant
                    }
                }
            };
            previous_variant_name = ancestor_ident;
        }

        quote! {
            #into_implementations
        }
    } else {
        quote! {}
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #interfaces_implementation

        #into_implementations
    }
    .into()
}

fn get_enum_unwrapped_match(argument: &Expr, ancestors: &[Ident]) -> TokenStream2 {
    let mut unwrapped_variant_selector = quote! {
        unwrapped
    };
    for (index, ancestor) in ancestors.iter().skip(1).rev().enumerate() {
        let previous_ancestor = &ancestors[ancestors.len() - 2 - index];
        unwrapped_variant_selector = quote! {
            #previous_ancestor::#ancestor(#unwrapped_variant_selector)
        }
    }

    let last_variant = ancestors.last().unwrap();

    quote! {
        match #argument {
            #unwrapped_variant_selector => unwrapped,
            _ => panic!("Expected {}", stringify!(#last_variant)),
        }
    }
}

fn expr_to_ident(expr: &Expr) -> Result<Ident> {
    match expr {
        Expr::Path(path) => match path.path.get_ident() {
            Some(ident) => Ok(ident.clone()),
            None => Err(Error::new_spanned(path, "Expected ident")),
        },
        _ => Err(Error::new_spanned(expr, "Expected ident")),
    }
}

struct EnumUnwrapped {
    argument: Expr,
    ancestors: Vec<Ident>,
}

impl Parse for EnumUnwrapped {
    fn parse(input: ParseStream) -> Result<Self> {
        let argument: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let ancestors_expr: ExprArray = input.parse()?;
        // let ancestors = ancestors
        //     .elems
        //     .iter()
        //     .map(|expr| expr_to_ident(expr)?)
        //     .collect::<Vec<_>>();
        let mut ancestors = vec![];
        for ancestor in ancestors_expr.elems.iter() {
            ancestors.push(expr_to_ident(ancestor)?);
        }
        Ok(EnumUnwrapped {
            argument,
            ancestors,
        })
    }
}

#[proc_macro]
pub fn enum_unwrapped(input: TokenStream) -> TokenStream {
    let EnumUnwrapped {
        argument,
        ancestors,
    } = parse_macro_input!(input as EnumUnwrapped);

    let enum_match = get_enum_unwrapped_match(&argument, &ancestors);

    quote! {
        #enum_match
    }
    .into()
}

fn get_node_enum_unwrapped_call(argument: &Expr, variant_name: &Ident) -> TokenStream2 {
    let known_node_variant_names: HashMap<String, Vec<&'static str>> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("Expression".to_string(), vec![]),
            ("VariableDeclarationList".to_string(), vec![]),
            ("TypeParameterDeclaration".to_string(), vec![]),
            ("PropertyAssignment".to_string(), vec![]),
        ]));

    let mut ancestors = vec!["Node"];
    let variant_name_string = variant_name.to_string();
    ancestors.extend_from_slice(known_node_variant_names.get(&variant_name_string).unwrap());
    ancestors.push(&variant_name_string);
    let ancestors = ancestors
        .into_iter()
        .map(|ancestor_str| Ident::new(ancestor_str, variant_name.span()));
    let ancestors = quote! {
        [#(#ancestors),*]
    };

    quote! {
        ::local_macros::enum_unwrapped!(#argument, #ancestors)
    }
}

struct NodeUnwrapped {
    argument: Expr,
    variant_name: Ident,
}

impl Parse for NodeUnwrapped {
    fn parse(input: ParseStream) -> Result<Self> {
        let argument: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let variant_name: Ident = input.parse()?;
        Ok(NodeUnwrapped {
            argument,
            variant_name,
        })
    }
}

#[proc_macro]
pub fn node_unwrapped(input: TokenStream) -> TokenStream {
    let NodeUnwrapped {
        argument,
        variant_name,
    } = parse_macro_input!(input as NodeUnwrapped);

    let enum_unwrapped_call = get_node_enum_unwrapped_call(&argument, &variant_name);

    quote! {
        #enum_unwrapped_call
    }
    .into()
}

fn get_type_enum_unwrapped_call(argument: &Expr, variant_name: &Ident) -> TokenStream2 {
    let known_type_variant_names: HashMap<String, Vec<&'static str>> =
        HashMap::from_iter(IntoIterator::into_iter([(
            "BaseInterfaceType".to_string(),
            vec!["ObjectType", "InterfaceType"],
        )]));

    let mut ancestors = vec!["Type"];
    let variant_name_string = variant_name.to_string();
    ancestors.extend_from_slice(known_type_variant_names.get(&variant_name_string).unwrap());
    ancestors.push(&variant_name_string);
    let ancestors = ancestors
        .into_iter()
        .map(|ancestor_str| Ident::new(ancestor_str, variant_name.span()));
    let ancestors = quote! {
        [#(#ancestors),*]
    };

    quote! {
        ::local_macros::enum_unwrapped!(#argument, #ancestors)
    }
}

struct TypeUnwrapped {
    argument: Expr,
    variant_name: Ident,
}

impl Parse for TypeUnwrapped {
    fn parse(input: ParseStream) -> Result<Self> {
        let argument: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let variant_name: Ident = input.parse()?;
        Ok(TypeUnwrapped {
            argument,
            variant_name,
        })
    }
}

#[proc_macro]
pub fn type_unwrapped(input: TokenStream) -> TokenStream {
    let TypeUnwrapped {
        argument,
        variant_name,
    } = parse_macro_input!(input as TypeUnwrapped);

    let enum_unwrapped_call = get_type_enum_unwrapped_call(&argument, &variant_name);

    quote! {
        #enum_unwrapped_call
    }
    .into()
}

#[proc_macro_attribute]
pub fn generate_node_factory_method_wrapper(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let parsed_fn_item = parse_macro_input!(item_for_parsing as ItemFn);

    let raw_method_name_ident = &parsed_fn_item.sig.ident;
    let raw_method_name = raw_method_name_ident.to_string();
    assert!(raw_method_name.ends_with("_raw"));
    let wrapper_method_name = &raw_method_name[..raw_method_name.len() - 4];

    let mut wrapper_fn_item = parsed_fn_item.clone();
    wrapper_fn_item.sig.ident = Ident::new(wrapper_method_name, Span::call_site());

    let id_node_token_stream: TokenStream = quote!(-> id_arena::Id<crate::Node>).into();
    let id_node_return_type = parse_macro_input!(id_node_token_stream as ReturnType);
    wrapper_fn_item.sig.output = id_node_return_type;

    let forwarded_arguments = parsed_fn_item
        .sig
        .inputs
        .iter()
        .skip(1)
        .map(|param| match param {
            FnArg::Typed(pat_type) => match &*pat_type.pat {
                Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                _ => panic!("expected Pat::Ident"),
            },
            _ => panic!("expected FnArg::Typed"),
        })
        .collect::<Vec<_>>();

    let forwarded_arguments = quote!(#(#forwarded_arguments),*);
    let wrapper_fn_body_token_stream: TokenStream = quote! {
        {
            crate::HasArena::alloc_node(self, self.#raw_method_name_ident(#forwarded_arguments).into())
        }
    }
    .into();
    let wrapper_fn_body_block = parse_macro_input!(wrapper_fn_body_token_stream as Block);
    wrapper_fn_item.block = Box::new(wrapper_fn_body_block);

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #wrapper_fn_item
    }
    .into()
}
