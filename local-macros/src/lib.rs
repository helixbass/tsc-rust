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
        vec.append(&mut self.interfaces.as_ref().map_or_else(
            || vec![],
            |interfaces_str| {
                interfaces_str
                    .split(",")
                    .into_iter()
                    .map(|chunk| chunk.trim().to_string())
                    .collect()
            },
        ));
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
                    fn pos(&self) -> usize {
                        self.#first_field_name.pos()
                    }

                    fn set_pos(&self, pos: usize) {
                        self.#first_field_name.set_pos(pos);
                    }

                    fn end(&self) -> usize {
                        self.#first_field_name.end()
                    }

                    fn set_end(&self, end: usize) {
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
            let variant_names = variants.iter().map(|variant| &variant.ident);
            let variant_names_2 = variant_names.clone();
            let variant_names_3 = variant_names.clone();
            let variant_names_4 = variant_names.clone();
            let variant_names_5 = variant_names.clone();
            let variant_names_6 = variant_names.clone();
            let variant_names_7 = variant_names.clone();
            let variant_names_8 = variant_names.clone();
            let variant_names_9 = variant_names.clone();
            let variant_names_10 = variant_names.clone();
            let variant_names_11 = variant_names.clone();
            let variant_names_12 = variant_names.clone();
            let variant_names_13 = variant_names.clone();
            let variant_names_14 = variant_names.clone();

            quote! {
                impl crate::NodeInterface for #ast_type_name {
                    fn node_wrapper(&self) -> ::std::rc::Rc<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names_13(nested) => nested.node_wrapper()),*
                        }
                    }

                    fn set_node_wrapper(&self, wrapper: ::std::rc::Rc<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names_14(nested) => nested.set_node_wrapper(wrapper)),*
                        }
                    }

                    fn kind(&self) -> crate::SyntaxKind {
                        match self {
                            #(#ast_type_name::#variant_names(nested) => nested.kind()),*
                        }
                    }

                    fn parent(&self) -> ::std::rc::Rc<crate::Node> {
                        match self {
                            #(#ast_type_name::#variant_names_2(nested_2) => nested_2.parent()),*
                        }
                    }

                    fn set_parent(&self, parent: ::std::rc::Rc<crate::Node>) {
                        match self {
                            #(#ast_type_name::#variant_names_3(nested_3) => nested_3.set_parent(parent)),*
                        }
                    }

                    fn maybe_symbol(&self) -> ::std::option::Option<::std::rc::Rc<crate::Symbol>> {
                        match self {
                            #(#ast_type_name::#variant_names_12(nested_12) => nested_12.maybe_symbol()),*
                        }
                    }

                    fn symbol(&self) -> ::std::rc::Rc<crate::Symbol> {
                        match self {
                            #(#ast_type_name::#variant_names_4(nested_4) => nested_4.symbol()),*
                        }
                    }

                    fn set_symbol(&self, symbol: ::std::rc::Rc<crate::Symbol>) {
                        match self {
                            #(#ast_type_name::#variant_names_5(nested_5) => nested_5.set_symbol(symbol)),*
                        }
                    }

                    fn locals(&self) -> ::std::cell::RefMut<crate::SymbolTable> {
                        match self {
                            #(#ast_type_name::#variant_names_6(nested_6) => nested_6.locals()),*
                        }
                    }

                    fn set_locals(&self, locals: crate::SymbolTable) {
                        match self {
                            #(#ast_type_name::#variant_names_7(nested_7) => nested_7.set_locals(locals)),*
                        }
                    }
                }

                impl crate::ReadonlyTextRange for #ast_type_name {
                    fn pos(&self) -> usize {
                        match self {
                            #(#ast_type_name::#variant_names_8(nested_8) => nested_8.pos()),*
                        }
                    }

                    fn set_pos(&self, pos: usize) {
                        match self {
                            #(#ast_type_name::#variant_names_9(nested_9) => nested_9.set_pos(pos)),*
                        }
                    }

                    fn end(&self) -> usize {
                        match self {
                            #(#ast_type_name::#variant_names_10(nested_10) => nested_10.end()),*
                        }
                    }

                    fn set_end(&self, end: usize) {
                        match self {
                            #(#ast_type_name::#variant_names_11(nested_11) => nested_11.set_end(end)),*
                        }
                    }
                }
            }
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
