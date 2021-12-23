use proc_macro::TokenStream;
use quote::quote;
use syn::Data::{Enum, Struct};
use syn::{parse_macro_input, DataEnum, DeriveInput, Fields, FieldsNamed};

#[proc_macro_attribute]
pub fn ast_type(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: ast_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let node_interface_and_readonly_text_range_implementation = match data {
        Struct(struct_) => {
            let first_field_name = match struct_.fields {
                Fields::Named(FieldsNamed { named, .. }) => named
                    .iter()
                    .nth(0)
                    .expect("Expected at least one struct field")
                    .ident
                    .clone(),
                _ => panic!("Expected named fields"),
            };

            quote! {
                impl crate::NodeInterface for #ast_type_name {
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

                    fn locals(&self) -> ::parking_lot::MappedRwLockWriteGuard<crate::SymbolTable> {
                        self.#first_field_name.locals()
                    }

                    fn set_locals(&self, locals: crate::SymbolTable) {
                        self.#first_field_name.set_locals(locals)
                    }
                }

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

            quote! {
                impl crate::NodeInterface for #ast_type_name {
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

                    fn locals(&self) -> ::parking_lot::MappedRwLockWriteGuard<crate::SymbolTable> {
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

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #node_interface_and_readonly_text_range_implementation
    }
    .into()
}
