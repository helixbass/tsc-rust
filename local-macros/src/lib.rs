use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use std::array::IntoIter;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream, Result};
use syn::Data::{Enum, Struct};
use syn::{
    parse_macro_input, AttributeArgs, DataEnum, DeriveInput, Error, Expr, ExprArray, Fields,
    FieldsNamed, Token,
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
        vec.push("Node".to_string());
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
) -> TokenStream2 {
    match interface_name {
        "NodeInterface" => {
            quote! {
                impl crate::NodeInterface for #ast_type_name {
                    fn node_wrapper(&self) -> ::std::rc::Rc<crate::Node> {
                        self.#first_field_name.node_wrapper()
                    }

                    fn set_node_wrapper(&self, wrapper: ::std::rc::Rc<crate::Node>) {
                        self.#first_field_name.set_node_wrapper(wrapper)
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

                    fn transform_flags(&self) -> crate::TransformFlags {
                        self.#first_field_name.transform_flags()
                    }

                    fn set_transform_flags(&mut self, flags: crate::TransformFlags) {
                        self.#first_field_name.set_transform_flags(flags)
                    }

                    fn add_transform_flags(&mut self, flags: crate::TransformFlags) {
                        self.#first_field_name.add_transform_flags(flags)
                    }

                    fn maybe_decorators(&self) -> ::std::cell::Ref<::std::option::Option<crate::NodeArray>> {
                        self.#first_field_name.maybe_decorators()
                    }

                    fn set_decorators(&self, decorators: ::std::option::Option<crate::NodeArray>) {
                        self.#first_field_name.set_decorators(decorators)
                    }

                    fn maybe_modifiers(&self) -> ::std::option::Option<&crate::NodeArray> {
                        self.#first_field_name.maybe_modifiers()
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

                    fn maybe_parent(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_parent()
                    }

                    fn parent(&self) -> ::std::rc::Rc<crate::Node> {
                        self.#first_field_name.parent()
                    }

                    fn set_parent(&self, parent: ::std::rc::Rc<crate::Node>) {
                        self.#first_field_name.set_parent(parent)
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::std::rc::Rc<crate::Symbol>> {
                        self.#first_field_name.maybe_symbol()
                    }

                    fn symbol(&self) -> ::std::rc::Rc<crate::Symbol> {
                        self.#first_field_name.symbol()
                    }

                    fn set_symbol(&self, symbol: ::std::rc::Rc<crate::Symbol>) {
                        self.#first_field_name.set_symbol(symbol);
                    }

                    fn maybe_locals(&self) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        self.#first_field_name.maybe_locals()
                    }

                    fn locals(&self) -> ::std::cell::RefMut<crate::SymbolTable> {
                        self.#first_field_name.locals()
                    }

                    fn set_locals(&self, locals: ::std::option::Option<crate::SymbolTable>) {
                        self.#first_field_name.set_locals(locals)
                    }

                    fn maybe_js_doc(&self) -> ::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>> {
                        self.#first_field_name.maybe_js_doc()
                    }

                    fn set_js_doc(&self, js_doc: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        self.#first_field_name.set_js_doc(js_doc)
                    }

                    fn maybe_js_doc_cache(&self) -> ::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>> {
                        self.#first_field_name.maybe_js_doc_cache()
                    }

                    fn set_js_doc_cache(&self, js_doc_cache: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        self.#first_field_name.set_js_doc_cache(js_doc_cache)
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
                    fn maybe_name(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_name()
                    }

                    fn name(&self) -> ::std::rc::Rc<crate::Node> {
                        self.#first_field_name.name()
                    }

                    fn set_name(&mut self, name: ::std::rc::Rc<crate::Node>) {
                        self.#first_field_name.set_name(name);
                    }
                }
            }
        }
        "HasTypeParametersInterface" => {
            quote! {
                impl crate::HasTypeParametersInterface for #ast_type_name {
                    fn maybe_type_parameters(&self) -> ::std::option::Option<&crate::NodeArray> {
                        self.#first_field_name.maybe_type_parameters()
                    }
                }
            }
        }
        "HasInitializerInterface" => {
            quote! {
                impl crate::HasInitializerInterface for #ast_type_name {
                    fn maybe_initializer(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_initializer()
                    }

                    fn set_initializer(&mut self, initializer: ::std::rc::Rc<crate::Node>) {
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
                    fn maybe_type(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_type()
                    }

                    fn set_type(&mut self, type_: ::std::rc::Rc<crate::Node>) {
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
                    fn text(&self) -> &str {
                        self.#first_field_name.text()
                    }

                    fn is_unterminated(&self) -> Option<bool> {
                        self.#first_field_name.is_unterminated()
                    }

                    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>) {
                        self.#first_field_name.set_is_unterminated(is_unterminated);
                    }

                    fn has_extended_unicode_escape(&self) -> Option<bool> {
                        self.#first_field_name.has_extended_unicode_escape()
                    }

                    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>) {
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
                    fn parameters(&self) -> &crate::NodeArray {
                        self.#first_field_name.parameters()
                    }
                }
            }
        }
        "FunctionLikeDeclarationInterface" => {
            quote! {
                impl crate::FunctionLikeDeclarationInterface for #ast_type_name {
                    fn maybe_body(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_body()
                    }

                    fn maybe_asterisk_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_asterisk_token()
                    }

                    fn maybe_question_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_question_token()
                    }

                    fn maybe_exclamation_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.maybe_exclamation_token()
                    }
                }
            }
        }
        "JSDocTagInterface" => {
            quote! {
                impl crate::JSDocTagInterface for #ast_type_name {
                    fn tag_name(&self) -> ::std::rc::Rc<crate::Node> {
                        self.#first_field_name.tag_name()
                    }

                    fn maybe_comment(&self) -> ::std::option::Option<&crate::StringOrNodeArray> {
                        self.#first_field_name.maybe_comment()
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
) -> TokenStream2 {
    match interface_name {
        "NodeInterface" => {
            quote! {
                impl crate::NodeInterface for #ast_type_name {
                    fn node_wrapper(&self) -> ::std::rc::Rc<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.node_wrapper()),*
                        }
                    }

                    fn set_node_wrapper(&self, wrapper: ::std::rc::Rc<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_node_wrapper(wrapper)),*
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

                    fn transform_flags(&self) -> crate::TransformFlags {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.transform_flags()),*
                        }
                    }

                    fn set_transform_flags(&mut self, flags: crate::TransformFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_transform_flags(flags)),*
                        }
                    }

                    fn add_transform_flags(&mut self, flags: crate::TransformFlags) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.add_transform_flags(flags)),*
                        }
                    }

                    fn maybe_decorators(&self) -> ::std::cell::Ref<::std::option::Option<crate::NodeArray>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_decorators()),*
                        }
                    }

                    fn set_decorators(&self, decorators: ::std::option::Option<crate::NodeArray>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_decorators(decorators)),*
                        }
                    }

                    fn maybe_modifiers(&self) -> ::std::option::Option<&crate::NodeArray> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_modifiers()),*
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

                    fn maybe_parent(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_parent()),*
                        }
                    }

                    fn parent(&self) -> ::std::rc::Rc<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.parent()),*
                        }
                    }

                    fn set_parent(&self, parent: ::std::rc::Rc<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_parent(parent)),*
                        }
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::std::rc::Rc<crate::Symbol>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_symbol()),*
                        }
                    }

                    fn symbol(&self) -> ::std::rc::Rc<crate::Symbol> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.symbol()),*
                        }
                    }

                    fn set_symbol(&self, symbol: ::std::rc::Rc<crate::Symbol>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_symbol(symbol)),*
                        }
                    }

                    fn maybe_locals(&self) -> ::std::cell::RefMut<::std::option::Option<crate::SymbolTable>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_locals()),*
                        }
                    }

                    fn locals(&self) -> ::std::cell::RefMut<crate::SymbolTable> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.locals()),*
                        }
                    }

                    fn set_locals(&self, locals: ::std::option::Option<crate::SymbolTable>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_locals(locals)),*
                        }
                    }

                    fn maybe_js_doc(&self) -> ::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_js_doc()),*
                        }
                    }

                    fn set_js_doc(&self, js_doc: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_js_doc(js_doc)),*
                        }
                    }

                    fn maybe_js_doc_cache(&self) -> ::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_js_doc_cache()),*
                        }
                    }

                    fn set_js_doc_cache(&self, js_doc_cache: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_js_doc_cache(js_doc_cache)),*
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
                    fn text(&self) -> &str {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.text()),*
                        }
                    }

                    fn is_unterminated(&self) -> Option<bool> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.is_unterminated()),*
                        }
                    }

                    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_is_unterminated(is_unterminated)),*
                        }
                    }

                    fn has_extended_unicode_escape(&self) -> Option<bool> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.has_extended_unicode_escape()),*
                        }
                    }

                    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>) {
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
                    fn maybe_name(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_name()),*
                        }
                    }

                    fn name(&self) -> ::std::rc::Rc<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.name()),*
                        }
                    }

                    fn set_name(&mut self, name: ::std::rc::Rc<crate::Node>) {
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
                    fn maybe_type_parameters(&self) -> ::std::option::Option<&crate::NodeArray> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_type_parameters()),*
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
                    fn parameters(&self) -> &crate::NodeArray {
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
                    fn maybe_body(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_body()),*
                        }
                    }

                    fn maybe_asterisk_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_asterisk_token()),*
                        }
                    }

                    fn maybe_question_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_question_token()),*
                        }
                    }

                    fn maybe_exclamation_token(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_exclamation_token()),*
                        }
                    }
                }
            }
        }
        "HasTypeInterface" => {
            quote! {
                impl crate::HasTypeInterface for #ast_type_name {
                    fn maybe_type(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.maybe_type()),*
                        }
                    }

                    fn set_type(&mut self, type_: ::std::rc::Rc<crate::Node>) {
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
                    fn tag_name(&self) -> ::std::rc::Rc<crate::Node> {
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
                let interface_impl =
                    get_ast_struct_interface_impl(&interface, &first_field_name, &ast_type_name);
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
                    get_ast_enum_interface_impl(&interface, &variant_names, &ast_type_name);
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
        for ancestor in args.ancestors_vec() {
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

            impl ::std::convert::From<#ast_type_name> for ::std::rc::Rc<crate::Node> {
                fn from(concrete: #ast_type_name) -> Self {
                    let rc = ::std::rc::Rc::new(#construct_variant);
                    crate::NodeInterface::set_node_wrapper(&*rc, rc.clone());
                    rc
                }
            }
        }
    } else {
        quote! {}
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
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
                    .split(",")
                    .into_iter()
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
                    .split(",")
                    .into_iter()
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
                    fn type_wrapper(&self) -> ::std::rc::Rc<crate::Type> {
                        self.#first_field_name.type_wrapper()
                    }

                    fn set_type_wrapper(&self, wrapper: ::std::rc::Rc<crate::Type>) {
                        self.#first_field_name.set_type_wrapper(wrapper)
                    }

                    fn flags(&self) -> crate::TypeFlags {
                        self.#first_field_name.flags()
                    }

                    fn id(&self) -> crate::TypeId {
                        self.#first_field_name.id()
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::std::rc::Rc<crate::Symbol>> {
                        self.#first_field_name.maybe_symbol()
                    }

                    fn symbol(&self) -> ::std::rc::Rc<crate::Symbol> {
                        self.#first_field_name.symbol()
                    }

                    fn set_symbol(&mut self, symbol: ::std::rc::Rc<crate::Symbol>) {
                        self.#first_field_name.set_symbol(symbol)
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
                    fn fresh_type(&self) -> ::std::option::Option<&::std::rc::Weak<crate::Type>> {
                        self.#first_field_name.fresh_type()
                    }

                    fn set_fresh_type(&self, fresh_type: &::std::rc::Rc<crate::Type>) {
                        self.#first_field_name.set_fresh_type(fresh_type)
                    }

                    fn get_or_initialize_fresh_type(
                        &self,
                        type_checker: &crate::TypeChecker,
                    ) -> Rc<Type> {
                        self.#first_field_name.get_or_initialize_fresh_type(type_checker)
                    }

                    fn regular_type(&self) -> ::std::rc::Rc<crate::Type> {
                        self.#first_field_name.regular_type()
                    }

                    fn set_regular_type(&self, regular_type: &::std::rc::Rc<crate::Type>) {
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
                impl crate::ObjectTypeInterface for #type_type_name {}
            }
        }
        "ResolvableTypeInterface" => {
            quote! {
                impl crate::ResolvableTypeInterface for #type_type_name {
                    fn resolve(&self, members: ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>>, properties: ::std::vec::Vec<::std::rc::Rc<crate::Symbol>>) {
                        self.#first_field_name.resolve(members, properties)
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
                    fn members(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>> {
                        self.#first_field_name.members()
                    }

                    fn properties(&self) -> ::std::cell::RefMut<::std::vec::Vec<::std::rc::Rc<crate::Symbol>>> {
                        self.#first_field_name.properties()
                    }

                    fn set_properties(&self, properties: ::std::vec::Vec<::std::rc::Rc<crate::Symbol>>) {
                        self.#first_field_name.set_properties(properties)
                    }
                }
            }
        }
        "UnionOrIntersectionTypeInterface" => {
            quote! {
                impl crate::UnionOrIntersectionTypeInterface for #type_type_name {
                    fn types(&self) -> &[::std::rc::Rc<crate::Type>] {
                        self.#first_field_name.types()
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
                    fn type_wrapper(&self) -> ::std::rc::Rc<crate::Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.type_wrapper()),*
                        }
                    }

                    fn set_type_wrapper(&self, wrapper: ::std::rc::Rc<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_type_wrapper(wrapper)),*
                        }
                    }

                    fn flags(&self) -> crate::TypeFlags {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.flags()),*
                        }
                    }

                    fn id(&self) -> crate::TypeId {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.id()),*
                        }
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::std::rc::Rc<crate::Symbol>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.maybe_symbol()),*
                        }
                    }

                    fn symbol(&self) -> ::std::rc::Rc<crate::Symbol> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.symbol()),*
                        }
                    }

                    fn set_symbol(&mut self, symbol: ::std::rc::Rc<crate::Symbol>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_symbol(symbol)),*
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
                    fn fresh_type(&self) -> ::std::option::Option<&::std::rc::Weak<crate::Type>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.fresh_type()),*
                        }
                    }

                    fn set_fresh_type(&self, fresh_type: &::std::rc::Rc<crate::Type>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_fresh_type(fresh_type)),*
                        }
                    }

                    fn get_or_initialize_fresh_type(
                        &self,
                        type_checker: &crate::TypeChecker,
                    ) -> Rc<Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.get_or_initialize_fresh_type(type_checker)),*
                        }
                    }

                    fn regular_type(&self) -> ::std::rc::Rc<crate::Type> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.regular_type()),*
                        }
                    }

                    fn set_regular_type(&self, regular_type: &::std::rc::Rc<crate::Type>) {
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
                impl crate::ObjectTypeInterface for #type_type_name {}
            }
        }
        "ResolvableTypeInterface" => {
            quote! {
                impl crate::ResolvableTypeInterface for #type_type_name {
                    fn resolve(&self, members: ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>>, properties: ::std::vec::Vec<::std::rc::Rc<crate::Symbol>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.resolve(members, properties)),*
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
                    fn members(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.members()),*
                        }
                    }

                    fn properties(&self) -> ::std::cell::RefMut<::std::vec::Vec<::std::rc::Rc<crate::Symbol>>> {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.properties()),*
                        }
                    }

                    fn set_properties(&self, properties: ::std::vec::Vec<::std::rc::Rc<crate::Symbol>>) {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.set_properties(properties)),*
                        }
                    }
                }
            }
        }
        "UnionOrIntersectionTypeInterface" => {
            quote! {
                impl crate::UnionOrIntersectionTypeInterface for #type_type_name {
                    fn types(&self) -> &[::std::rc::Rc<crate::Type>] {
                        match self {
                            #(#type_type_name::#variant_names(nested) => nested.types()),*
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

            impl ::std::convert::From<#type_type_name> for ::std::rc::Rc<crate::Type> {
                fn from(concrete: #type_type_name) -> Self {
                    let rc = ::std::rc::Rc::new(#construct_variant);
                    crate::TypeInterface::set_type_wrapper(&*rc, rc.clone());
                    rc
                }
            }
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
                    fn symbol_wrapper(&self) -> ::std::rc::Rc<crate::Symbol> {
                        self.#first_field_name.symbol_wrapper()
                    }

                    fn set_symbol_wrapper(&self, wrapper: ::std::rc::Rc<crate::Symbol>) {
                        self.#first_field_name.set_symbol_wrapper(wrapper)
                    }

                    fn flags(&self) -> crate::SymbolFlags {
                        self.#first_field_name.flags()
                    }

                    fn set_flags(&self, flags: crate::SymbolFlags) {
                        self.#first_field_name.set_flags(flags)
                    }

                    fn escaped_name(&self) -> &crate::__String {
                        self.#first_field_name.escaped_name()
                    }

                    fn maybe_declarations(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>>> {
                        self.#first_field_name.maybe_declarations()
                    }

                    fn set_declarations(&self, declarations: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        self.#first_field_name.set_declarations(declarations)
                    }

                    fn maybe_value_declaration(&self) -> ::std::cell::Ref<::std::option::Option<::std::rc::Weak<crate::Node>>> {
                        self.#first_field_name.maybe_value_declaration()
                    }

                    fn set_value_declaration(&self, node: ::std::rc::Rc<crate::Node>) {
                        self.#first_field_name.set_value_declaration(node)
                    }

                    fn maybe_members(&self) -> ::std::cell::RefMut<::std::option::Option<::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>>>> {
                        self.#first_field_name.maybe_members()
                    }

                    fn members(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>> {
                        self.#first_field_name.members()
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
                }
            }
        }
        "TransientSymbolInterface" => {
            quote! {
                impl crate::TransientSymbolInterface for #symbol_type_name {
                    fn symbol_links(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolLinks>> {
                        self.#first_field_name.symbol_links()
                    }

                    fn check_flags(&self) -> crate::CheckFlags {
                        self.#first_field_name.check_flags()
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
                    fn symbol_wrapper(&self) -> ::std::rc::Rc<crate::Symbol> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.symbol_wrapper()),*
                        }
                    }

                    fn set_symbol_wrapper(&self, wrapper: ::std::rc::Rc<crate::Symbol>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_symbol_wrapper(wrapper)),*
                        }
                    }

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

                    fn escaped_name(&self) -> &crate::__String {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.escaped_name()),*
                        }
                    }

                    fn maybe_declarations(&self) -> ::std::cell::Ref<::std::option::Option<::std::vec::Vec<::std::rc::Rc<crate::Node>>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_declarations()),*
                        }
                    }

                    fn set_declarations(&self, declarations: ::std::vec::Vec<::std::rc::Rc<crate::Node>>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_declarations(declarations)),*
                        }
                    }

                    fn maybe_value_declaration(&self) -> ::std::cell::Ref<::std::option::Option<::std::rc::Weak<crate::Node>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_value_declaration()),*
                        }
                    }

                    fn set_value_declaration(&self, node: ::std::rc::Rc<crate::Node>) {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.set_value_declaration(node)),*
                        }
                    }

                    fn maybe_members(&self) -> ::std::cell::RefMut<::std::option::Option<::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>>>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.maybe_members()),*
                        }
                    }

                    fn members(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolTable>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.members()),*
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
                }
            }
        }
        "TransientSymbolInterface" => {
            quote! {
                impl crate::TransientSymbolInterface for #symbol_type_name {
                    fn symbol_links(&self) -> ::std::rc::Rc<::std::cell::RefCell<crate::SymbolLinks>> {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.symbol_links()),*
                        }
                    }

                    fn check_flags(&self) -> crate::CheckFlags {
                        match self {
                            #(#symbol_type_name::#variant_names(nested) => nested.check_flags()),*
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

            impl ::std::convert::From<#symbol_type_name> for ::std::rc::Rc<crate::Symbol> {
                fn from(concrete: #symbol_type_name) -> Self {
                    let rc = ::std::rc::Rc::new(#construct_variant);
                    crate::SymbolInterface::set_symbol_wrapper(&*rc, rc.clone());
                    rc
                }
            }
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
                    fn command_line_option_wrapper(&self) -> ::std::rc::Rc<crate::CommandLineOption> {
                        self.#first_field_name.command_line_option_wrapper()
                    }

                    fn set_command_line_option_wrapper(&self, wrapper: ::std::rc::Rc<crate::CommandLineOption>) {
                        self.#first_field_name.set_command_line_option_wrapper(wrapper)
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

                    fn transpile_option_value(&self) -> bool {
                        self.#first_field_name.transpile_option_value()
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
                    fn command_line_option_wrapper(&self) -> ::std::rc::Rc<crate::CommandLineOption> {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.command_line_option_wrapper()),*
                        }
                    }

                    fn set_command_line_option_wrapper(&self, wrapper: ::std::rc::Rc<crate::CommandLineOption>) {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.set_command_line_option_wrapper(wrapper)),*
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

                    fn transpile_option_value(&self) -> bool {
                        match self {
                            #(#command_line_option_type_name::#variant_names(nested) => nested.transpile_option_value()),*
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

            impl ::std::convert::From<#command_line_option_type_name> for ::std::rc::Rc<crate::CommandLineOption> {
                fn from(concrete: #command_line_option_type_name) -> Self {
                    let rc = ::std::rc::Rc::new(#construct_variant);
                    crate::CommandLineOptionInterface::set_command_line_option_wrapper(&*rc, rc.clone());
                    rc
                }
            }
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
            crate::#previous_ancestor::#ancestor(#unwrapped_variant_selector)
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
        HashMap::from_iter(IntoIter::new([
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
        HashMap::from_iter(IntoIter::new([(
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
