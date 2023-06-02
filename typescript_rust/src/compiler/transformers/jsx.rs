use std::{collections::HashMap, io, rc::Rc};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    chain_bundle, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped, BaseNodeFactorySynthetic,
    CompilerOptions, EmitHelperFactory, GeneratedIdentifierFlags, JsxEmit,
    NamedDeclarationInterface, Node, NodeFactory, NodeInterface, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, _d,
    get_jsx_runtime_import, NodeArray,
};

#[derive(Trace, Finalize)]
pub(super) struct PerFileState {
    pub import_specifier: Option<String>,
    pub filename_declaration: Option<Gc<Node /*VariableDeclaration & { name: Identifier; }*/>>,
    pub utilized_implicit_runtime_imports:
        Option<HashMap<String, HashMap<String, Gc<Node /*ImportSpecifier*/>>>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformJsx {
    context: Gc<Box<dyn TransformationContext>>,
    compiler_options: Gc<CompilerOptions>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    current_file_state: GcCell<Option<PerFileState>>,
}

impl TransformJsx {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self {
            factory: context.factory(),
            compiler_options: context.get_compiler_options(),
            context,
            current_source_file: _d(),
            current_file_state: _d(),
        }
    }

    pub(super) fn current_source_file(&self) -> GcCellRef<Gc<Node /*SourceFile*/>> {
        gc_cell_ref_unwrapped(&self.current_source_file)
    }

    pub(super) fn current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>, Gc<Node /*SourceFile*/>> {
        gc_cell_ref_mut_unwrapped(&self.current_source_file)
    }

    pub(super) fn current_file_state(&self) -> GcCellRef<PerFileState> {
        gc_cell_ref_unwrapped(&self.current_file_state)
    }

    pub(super) fn current_file_state_mut(
        &self,
    ) -> GcCellRefMut<Option<PerFileState>, PerFileState> {
        gc_cell_ref_mut_unwrapped(&self.current_file_state)
    }

    fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn get_current_file_name_expression(&self) -> Gc<Node /*Identifier*/> {
        if let Some(current_file_state_filename_declaration) =
            self.current_file_state().filename_declaration.clone()
        {
            return current_file_state_filename_declaration
                .as_variable_declaration()
                .name();
        }
        let declaration = self
            .factory
            .create_variable_declaration(
                Some(self.factory.create_unique_name(
                    "_jsxFileName",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                )),
                None,
                None,
                Some(
                    self.factory
                        .create_string_literal(
                            self.current_source_file()
                                .as_source_file()
                                .file_name()
                                .clone(),
                            None,
                            None,
                        )
                        .wrap(),
                ),
            )
            .wrap();
        self.current_file_state_mut().filename_declaration = Some(declaration.clone());
        declaration.as_variable_declaration().name()
    }

    fn get_jsx_factory_callee_primitive(&self, is_static_children: bool) -> &'static str /*"jsx" | "jsxs" | "jsxDEV"*/
    {
        if self.compiler_options.jsx == Some(JsxEmit::ReactJSXDev) {
            "jsxDEV"
        } else if is_static_children {
            "jsxs"
        } else {
            "jsx"
        }
    }

    fn get_jsx_factory_callee(&self, is_static_children: bool) -> Gc<Node> {
        let type_ = self.get_jsx_factory_callee_primitive(is_static_children);
        self.get_implicit_import_for_name(type_)
    }

    fn get_implicit_jsx_fragment_reference(&self) -> Gc<Node> {
        self.get_implicit_import_for_name("Fragment")
    }

    fn get_implicit_import_for_name(&self, name: &str) -> Gc<Node> {
        let import_source = if name == "createElement" {
            self.current_file_state().import_specifier.clone().unwrap()
        } else {
            get_jsx_runtime_import(
                self.current_file_state().import_specifier.as_deref(),
                &self.compiler_options,
            )
            .unwrap()
        };
        let existing = self
            .current_file_state()
            .utilized_implicit_runtime_imports
            .as_ref()
            .and_then(|current_file_state_utilized_implicit_runtime_imports| {
                current_file_state_utilized_implicit_runtime_imports.get(&import_source)
            })
            .and_then(
                |current_file_state_utilized_implicit_runtime_imports_value| {
                    current_file_state_utilized_implicit_runtime_imports_value.get(name)
                },
            )
            .cloned();
        if let Some(existing) = existing {
            return existing.as_import_specifier().name.clone();
        }
        let mut current_file_state = self.current_file_state_mut();
        let specifier_source_imports = current_file_state
            .utilized_implicit_runtime_imports
            .get_or_insert_with(|| _d())
            .entry(import_source)
            .or_insert_with(|| _d());
        let generated_name = self.factory.create_unique_name(
            &format!("_{name}"),
            Some(
                GeneratedIdentifierFlags::Optimistic
                    | GeneratedIdentifierFlags::FileLevel
                    | GeneratedIdentifierFlags::AllowNameSubstitution,
            ),
        );
        let specifier = self
            .factory
            .create_import_specifier(
                false,
                Some(
                    self.factory
                        .create_identifier(name, Option::<Gc<NodeArray>>::None, None)
                        .wrap(),
                ),
                generated_name.clone(),
            )
            .wrap();
        generated_name
            .as_identifier()
            .set_generated_import_reference(Some(specifier.clone()));
        specifier_source_imports.insert(name.to_owned(), specifier);
        generated_name
    }

    fn transform_source_file(&self, _node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformJsx {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformJsxFactory {}

impl TransformJsxFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformJsxFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformJsx::new(context))),
        )
    }
}

pub fn transform_jsx() -> TransformerFactory {
    Gc::new(Box::new(TransformJsxFactory::new()))
}
