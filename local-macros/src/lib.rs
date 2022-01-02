use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::Data::{Enum, Struct};
use syn::{parse_macro_input, AttributeArgs, DataEnum, DeriveInput, Fields, FieldsNamed};

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

fn get_struct_interface_impl(
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

                    fn set_locals(&self, locals: crate::SymbolTable) {
                        self.#first_field_name.set_locals(locals)
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
        "HasExpressionInitializerInterface" => {
            quote! {
                impl crate::HasExpressionInitializerInterface for #ast_type_name {
                    fn initializer(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self.#first_field_name.initializer()
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
                    fn type_(&self) -> ::std::option::Option<::std::rc::Rc<crate::Node>> {
                        self._variable_like_declaration.type_()
                    }

                    fn set_type(&mut self, type_: ::std::rc::Rc<crate::Node>) {
                        self._variable_like_declaration.set_type(type_);
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
                        self._literal_like_node.text()
                    }
                }
            }
        }
        _ => panic!("Unknown interface: {}", interface_name),
    }
}

fn get_enum_interface_impl(
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

                    fn set_locals(&self, locals: crate::SymbolTable) {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.set_locals(locals)),*
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
                }
            }
        }
        "NamedDeclarationInterface" => {
            quote! {
                impl crate::NamedDeclarationInterface for #ast_type_name {
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
                    get_struct_interface_impl(&interface, &first_field_name, &ast_type_name);
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
                    get_enum_interface_impl(&interface, &variant_names, &ast_type_name);
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
