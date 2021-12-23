use proc_macro::TokenStream;
use quote::quote;
use syn::Data::Struct;
use syn::{parse_macro_input, DeriveInput, Fields, FieldsNamed};

#[proc_macro_attribute]
pub fn ast_type(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_for_parsing = item.clone();
    let DeriveInput {
        ident: ast_type_name,
        data,
        ..
    } = parse_macro_input!(item_for_parsing);

    let node_interface_implementation = match data {
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
            }
        }
        _ => panic!("Expected struct"),
    };

    let item_as_proc_macro2_token_stream = proc_macro2::TokenStream::from(item);

    quote! {
        #item_as_proc_macro2_token_stream

        #node_interface_implementation
    }
    .into()
}
