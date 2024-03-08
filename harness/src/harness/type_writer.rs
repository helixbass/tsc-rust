use typescript_rust::{
    id_arena::Id, AllArenas, HasArena, InArena, Node, Program, SyntaxKind, TypeChecker, _d,
    for_each_child, get_source_text_of_node_from_source_file, impl_has_arena, is_binding_element,
    is_declaration_name, is_expression_node,
    is_expression_with_type_arguments_in_class_extends_clause, is_global_scope_augmentation,
    is_identifier, is_meta_property, is_module_declaration, is_part_of_type_node,
    is_property_access_or_qualified_name, skip_trivia, NodeArray, NodeInterface, ReadonlyTextRange,
    ScriptReferenceHost, SourceFileLike, TypeFlags, TypeFormatFlags,
};
use typescript_services_rust::{get_meaning_from_declaration, is_label_name, SemanticMeaning};

pub struct TypeWriterTypeResult {
    pub line: usize,
    pub syntax_kind: SyntaxKind,
    pub source_text: String,
    pub type_: String,
}

pub struct TypeWriterSymbolResult {
    pub line: usize,
    pub syntax_kind: SyntaxKind,
    pub source_text: String,
    pub symbol: String,
}

pub enum TypeWriterResult {
    Symbol(TypeWriterSymbolResult),
    Type(TypeWriterTypeResult),
}

struct ForEachAstNode {
    arena: *const AllArenas,
    work: Vec<Id<Node>>,
}

impl ForEachAstNode {
    pub fn new(node: Id<Node>, arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
            work: vec![node],
        }
    }
}

impl Iterator for ForEachAstNode {
    type Item = Id<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.work.is_empty() {
            return None;
        }
        let elem = self.work.pop().unwrap();

        let mut res_children: Vec<Id<Node>> = _d();
        for_each_child(
            elem,
            |c| {
                res_children.insert(0, c);
            },
            Option::<fn(Id<NodeArray>)>::None,
            self,
        );
        self.work.extend(res_children);
        Some(elem)
    }
}

impl_has_arena!(ForEachAstNode);

pub struct TypeWriterWalker {
    arena: *const AllArenas,
    current_source_file: Option<Id<Node /*SourceFile*/>>,
    checker: Id<TypeChecker>,
    program: Id<Program>,
    had_error_baseline: bool,
}

impl TypeWriterWalker {
    pub fn new(
        program: Id<Program>,
        full_type_check: bool,
        had_error_baseline: bool,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            current_source_file: _d(),
            had_error_baseline,
            checker: if full_type_check {
                program
                    .ref_(arena)
                    .get_diagnostics_producing_type_checker()
                    .unwrap()
            } else {
                program.ref_(arena).get_type_checker().unwrap()
            },
            program,
        }
    }

    fn current_source_file(&self) -> Id<Node> {
        self.current_source_file.unwrap()
    }

    pub fn get_symbols(&mut self, file_name: &str) -> Vec<TypeWriterSymbolResult> {
        let source_file = self.program.ref_(self).get_source_file(file_name).unwrap();
        self.current_source_file = Some(source_file);
        self.visit_node(source_file, true)
            .into_iter()
            .map(|value| match value {
                TypeWriterResult::Symbol(value) => value,
                _ => unreachable!(),
            })
            .collect()
    }

    pub fn get_types(&mut self, file_name: &str) -> Vec<TypeWriterTypeResult> {
        let source_file = self.program.ref_(self).get_source_file(file_name).unwrap();
        self.current_source_file = Some(source_file);
        self.visit_node(source_file, true)
            .into_iter()
            .map(|value| match value {
                TypeWriterResult::Type(value) => value,
                _ => unreachable!(),
            })
            .collect()
    }

    fn visit_node(&self, node: Id<Node>, is_symbol_walk: bool) -> Vec<TypeWriterResult> {
        let mut ret: Vec<TypeWriterResult> = _d();
        for node in ForEachAstNode::new(node, self) {
            if is_expression_node(node, self)
                || node.ref_(self).kind() == SyntaxKind::Identifier
                || is_declaration_name(node, self)
            {
                if let Some(result) = self.write_type_or_symbol(node, is_symbol_walk) {
                    ret.push(result);
                }
            }
        }
        ret
    }

    fn write_type_or_symbol(
        &self,
        node: Id<Node>,
        is_symbol_walk: bool,
    ) -> Option<TypeWriterResult> {
        let actual_pos = skip_trivia(
            &self
                .current_source_file()
                .ref_(self)
                .as_source_file()
                .text_as_chars(),
            node.ref_(self).pos(),
            None,
            None,
            None,
        );
        let line_and_character = self
            .current_source_file()
            .ref_(self)
            .as_source_file()
            .get_line_and_character_of_position(usize::try_from(actual_pos).unwrap());
        let source_text =
            get_source_text_of_node_from_source_file(self.current_source_file(), node, None, self);

        if !is_symbol_walk {
            if is_part_of_type_node(node, self)
                || is_identifier(&node.ref_(self))
                    && !get_meaning_from_declaration(node.ref_(self).parent())
                        .intersects(SemanticMeaning::Value)
                    && !(is_type_alias_declaration(&node.ref_(self).parent())
                        && node
                            .ref_(self)
                            .parent()
                            .ref_(self)
                            .as_type_alias_declaration()
                            .name()
                            == node)
            {
                return None;
            }

            let type_ = is_expression_with_type_arguments_in_class_extends_clause(
                node.ref_(self).parent(),
                self,
            )
            .then(|| {
                self.checker
                    .ref_(self)
                    .get_type_at_location(node.ref_(self).parent())
                    .unwrap()
            })
            .filter(|type_| !type_.ref_(self).flags().intersects(TypeFlags::Any))
            .unwrap_or_else(|| self.checker.ref_(self).get_type_at_location(node).unwrap());
            let type_string = if !self.had_error_baseline
                && type_.ref_(self).flags().intersects(TypeFlags::Any)
                && !is_binding_element(&node.ref_(self).parent().ref_(self))
                && !is_property_access_or_qualified_name(&node.ref_(self).parent().ref_(self))
                && !is_label_name(node, self)
                && !(is_module_declaration(&node.ref_(self).parent().ref_(self))
                    && is_global_scope_augmentation(&node.ref_(self).parent().ref_(self)))
                && !is_meta_property(&node.ref_(self).parent().ref_(self))
                && !self.is_import_statement_name(node)
                && !self.is_export_statement_name(node)
                && !self.is_intrinsic_jsx_tag(node)
            {
                type_
                    .ref_(self)
                    .as_intrinsic_type()
                    .intrinsic_name()
                    .to_owned()
            } else {
                self.checker.ref_(self).type_to_string(
                    type_,
                    Some(node.ref_(self).parent()),
                    Some(TypeFormatFlags::NoTruncation | TypeFormatFlags::AllowUniqueESSymbolType),
                )
            };
            return Some(TypeWriterResult::Type(TypeWriterTypeResult {
                line: line_and_character.line,
                syntax_kind: node.ref_(self).kind(),
                source_text,
                type_: type_string,
            }));
        }

        unimplemented!()
    }
}

impl_has_arena!(TypeWriterWalker);
