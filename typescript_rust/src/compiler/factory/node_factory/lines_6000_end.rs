use std::{cell::RefCell, collections::HashMap, fmt};

use id_arena::Id;

use super::{create_node_factory, NodeFactoryFlags};
use crate::{
    add_range, create_base_node_factory, create_scanner, get_parse_node_factory, impl_has_arena,
    is_arrow_function, is_class_declaration, is_class_expression, is_constructor_declaration,
    is_enum_declaration, is_export_assignment, is_export_declaration, is_function_declaration,
    is_function_expression, is_get_accessor_declaration, is_import_declaration,
    is_import_equals_declaration, is_index_signature_declaration, is_interface_declaration,
    is_method_declaration, is_method_signature, is_module_declaration, is_named_declaration,
    is_parameter, is_property_declaration, is_property_name, is_property_signature,
    is_set_accessor_declaration, is_type_alias_declaration, is_variable_statement,
    maybe_append_if_unique_eq, per_arena, released, set_text_range, AllArenas, BaseNode,
    BaseNodeFactory, BaseNodeFactoryConcrete, BuildInfo, ClassLikeDeclarationInterface, Debug_,
    EmitFlags, EmitNode, FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface,
    HasMembersInterface, HasQuestionTokenInterface, HasTypeInterface, HasTypeParametersInterface,
    InArena, InputFiles, InterfaceOrClassLikeDeclarationInterface, LanguageVariant, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags,
    NodeInterface, PseudoBigInt, Scanner, ScriptTarget, SignatureDeclarationInterface,
    SourceMapRange, StrOrRcNode, StringOrBool, StringOrNumberOrBoolOrRcNode, StringOrRcNode,
    SyntaxKind, TransformFlags,
};

impl NodeFactory {
    pub fn update_modifiers(
        &self,
        node: Id<Node>, /*HasModifiers*/
        modifiers: impl Into<VecNodeOrModifierFlags>,
    ) -> Id<Node> {
        let modifiers = modifiers.into();
        let modifiers = match modifiers {
            VecNodeOrModifierFlags::VecNode(modifiers) => modifiers,
            VecNodeOrModifierFlags::ModifierFlags(modifiers) => {
                self.create_modifiers_from_modifier_flags(modifiers)
            }
        };
        if is_parameter(&node.ref_(self)) {
            self.update_parameter_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_parameter_declaration()
                    .dot_dot_dot_token
                    .clone()),
                released!(node.ref_(self).as_parameter_declaration().maybe_name()),
                released!(node
                    .ref_(self)
                    .as_parameter_declaration()
                    .maybe_question_token()),
                released!(node.ref_(self).as_parameter_declaration().maybe_type()),
                released!(node
                    .ref_(self)
                    .as_parameter_declaration()
                    .maybe_initializer()),
            )
        } else if is_property_signature(&node.ref_(self)) {
            self.update_property_signature(
                node,
                Some(modifiers),
                released!(node.ref_(self).as_property_signature().name()),
                released!(node
                    .ref_(self)
                    .as_property_signature()
                    .maybe_question_token()),
                released!(node.ref_(self).as_property_signature().maybe_type()),
            )
        } else if is_property_declaration(&node.ref_(self)) {
            self.update_property_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_property_declaration().name()),
                released!(node
                    .ref_(self)
                    .as_property_declaration()
                    .maybe_question_token()
                    .or_else(|| node
                        .ref_(self)
                        .as_property_declaration()
                        .exclamation_token
                        .clone())),
                released!(node.ref_(self).as_property_declaration().maybe_type()),
                released!(node
                    .ref_(self)
                    .as_property_declaration()
                    .maybe_initializer()),
            )
        } else if is_method_signature(&node.ref_(self)) {
            self.update_method_signature(
                node,
                Some(modifiers),
                released!(node.ref_(self).as_method_signature().name()),
                released!(node.ref_(self).as_method_signature().maybe_question_token()),
                released!(node
                    .ref_(self)
                    .as_method_signature()
                    .maybe_type_parameters()),
                released!(node.ref_(self).as_method_signature().parameters()),
                released!(node.ref_(self).as_method_signature().maybe_type()),
            )
        } else if is_method_declaration(&node.ref_(self)) {
            self.update_method_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_method_declaration()
                    .maybe_asterisk_token()),
                released!(node.ref_(self).as_method_declaration().name()),
                released!(node
                    .ref_(self)
                    .as_method_declaration()
                    .maybe_question_token()),
                released!(node
                    .ref_(self)
                    .as_method_declaration()
                    .maybe_type_parameters()),
                released!(node.ref_(self).as_method_declaration().parameters()),
                released!(node.ref_(self).as_method_declaration().maybe_type()),
                released!(node.ref_(self).as_method_declaration().maybe_body()),
            )
        } else if is_constructor_declaration(&node.ref_(self)) {
            self.update_constructor_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_constructor_declaration().parameters()),
                released!(node.ref_(self).as_constructor_declaration().maybe_body()),
            )
        } else if is_get_accessor_declaration(&node.ref_(self)) {
            self.update_get_accessor_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_get_accessor_declaration().name()),
                released!(node.ref_(self).as_get_accessor_declaration().parameters()),
                released!(node.ref_(self).as_get_accessor_declaration().maybe_type()),
                released!(node.ref_(self).as_get_accessor_declaration().maybe_body()),
            )
        } else if is_set_accessor_declaration(&node.ref_(self)) {
            self.update_set_accessor_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_set_accessor_declaration().name()),
                released!(node.ref_(self).as_set_accessor_declaration().parameters()),
                released!(node.ref_(self).as_set_accessor_declaration().maybe_body()),
            )
        } else if is_index_signature_declaration(&node.ref_(self)) {
            self.update_index_signature(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_index_signature_declaration()
                    .parameters()),
                released!(node
                    .ref_(self)
                    .as_index_signature_declaration()
                    .maybe_type()
                    .unwrap()),
            )
        } else if is_function_expression(&node.ref_(self)) {
            self.update_function_expression(
                node,
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_function_expression()
                    .maybe_asterisk_token()),
                released!(node.ref_(self).as_function_expression().maybe_name()),
                released!(node
                    .ref_(self)
                    .as_function_expression()
                    .maybe_type_parameters()),
                released!(node.ref_(self).as_function_expression().parameters()),
                released!(node.ref_(self).as_function_expression().maybe_type()),
                released!(node
                    .ref_(self)
                    .as_function_expression()
                    .maybe_body()
                    .unwrap()),
            )
        } else if is_arrow_function(&node.ref_(self)) {
            self.update_arrow_function(
                node,
                Some(modifiers),
                released!(node.ref_(self).as_arrow_function().maybe_type_parameters()),
                released!(node.ref_(self).as_arrow_function().parameters()),
                released!(node.ref_(self).as_arrow_function().maybe_type()),
                released!(node
                    .ref_(self)
                    .as_arrow_function()
                    .equals_greater_than_token
                    .clone()),
                released!(node.ref_(self).as_arrow_function().maybe_body().unwrap()),
            )
        } else if is_class_expression(&node.ref_(self)) {
            self.update_class_expression(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_class_expression().maybe_name()),
                released!(node
                    .ref_(self)
                    .as_class_expression()
                    .maybe_type_parameters()),
                released!(node
                    .ref_(self)
                    .as_class_expression()
                    .maybe_heritage_clauses()),
                released!(node.ref_(self).as_class_expression().members()),
            )
        } else if is_variable_statement(&node.ref_(self)) {
            self.update_variable_statement(
                node,
                Some(modifiers),
                released!(node.ref_(self).as_variable_statement().declaration_list),
            )
        } else if is_function_declaration(&node.ref_(self)) {
            self.update_function_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_function_declaration()
                    .maybe_asterisk_token()),
                released!(node.ref_(self).as_function_declaration().maybe_name()),
                released!(node
                    .ref_(self)
                    .as_function_declaration()
                    .maybe_type_parameters()),
                released!(node.ref_(self).as_function_declaration().parameters()),
                released!(node.ref_(self).as_function_declaration().maybe_type()),
                released!(node.ref_(self).as_function_declaration().maybe_body()),
            )
        } else if is_class_declaration(&node.ref_(self)) {
            self.update_class_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_class_declaration().maybe_name()),
                released!(node
                    .ref_(self)
                    .as_class_declaration()
                    .maybe_type_parameters()),
                released!(node
                    .ref_(self)
                    .as_class_declaration()
                    .maybe_heritage_clauses()),
                released!(node.ref_(self).as_class_declaration().members()),
            )
        } else if is_interface_declaration(&node.ref_(self)) {
            self.update_interface_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_interface_declaration().name()),
                released!(node
                    .ref_(self)
                    .as_interface_declaration()
                    .maybe_type_parameters()),
                released!(node
                    .ref_(self)
                    .as_interface_declaration()
                    .maybe_heritage_clauses()),
                released!(node.ref_(self).as_interface_declaration().members()),
            )
        } else if is_type_alias_declaration(&node.ref_(self)) {
            self.update_type_alias_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_type_alias_declaration().name()),
                released!(node
                    .ref_(self)
                    .as_type_alias_declaration()
                    .maybe_type_parameters()),
                released!(node
                    .ref_(self)
                    .as_type_alias_declaration()
                    .maybe_type()
                    .unwrap()),
            )
        } else if is_enum_declaration(&node.ref_(self)) {
            self.update_enum_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_enum_declaration().name()),
                released!(Some(node.ref_(self).as_enum_declaration().members.clone())),
            )
        } else if is_module_declaration(&node.ref_(self)) {
            self.update_module_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_module_declaration().name()),
                released!(node.ref_(self).as_module_declaration().body.clone()),
            )
        } else if is_import_equals_declaration(&node.ref_(self)) {
            self.update_import_equals_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_import_equals_declaration().is_type_only),
                released!(node.ref_(self).as_import_equals_declaration().name()),
                released!(node
                    .ref_(self)
                    .as_import_equals_declaration()
                    .module_reference
                    .clone()),
            )
        } else if is_import_declaration(&node.ref_(self)) {
            self.update_import_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node
                    .ref_(self)
                    .as_import_declaration()
                    .import_clause
                    .clone()),
                released!(node
                    .ref_(self)
                    .as_import_declaration()
                    .module_specifier
                    .clone()),
                released!(node
                    .ref_(self)
                    .as_import_declaration()
                    .assert_clause
                    .clone()),
            )
        } else if is_export_assignment(&node.ref_(self)) {
            self.update_export_assignment(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_export_assignment().expression.clone()),
            )
        } else if is_export_declaration(&node.ref_(self)) {
            self.update_export_declaration(
                node,
                released!(node.ref_(self).maybe_decorators()),
                Some(modifiers),
                released!(node.ref_(self).as_export_declaration().is_type_only),
                released!(node
                    .ref_(self)
                    .as_export_declaration()
                    .export_clause
                    .clone()),
                released!(node
                    .ref_(self)
                    .as_export_declaration()
                    .module_specifier
                    .clone()),
                released!(node
                    .ref_(self)
                    .as_export_declaration()
                    .assert_clause
                    .clone()),
            )
        } else {
            Debug_.assert_never(node, None)
        }
    }

    pub(super) fn as_node_array(
        &self,
        array: Option<impl Into<NodeArrayOrVec>>,
    ) -> Option<Id<NodeArray>> {
        array.map(|array| self.create_node_array(Some(array), None))
    }

    pub(super) fn as_name<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        name: Option<TName>,
    ) -> Option<Id<Node>> {
        name.map(|name| match name.into() {
            StrOrRcNode::Str(name) => self.create_identifier(name),
            StrOrRcNode::RcNode(name) => name,
        })
    }

    pub(super) fn as_expression(&self, value: impl Into<StringOrNumberOrBoolOrRcNode>) -> Id<Node> {
        match value.into() {
            StringOrNumberOrBoolOrRcNode::String(value) => {
                self.create_string_literal(value, None, None)
            }
            StringOrNumberOrBoolOrRcNode::Number(value) => self.create_numeric_literal(value, None),
            StringOrNumberOrBoolOrRcNode::Bool(value) => {
                if value {
                    self.create_true()
                } else {
                    self.create_false()
                }
            }
            StringOrNumberOrBoolOrRcNode::RcNode(value) => value,
        }
    }

    #[allow(dead_code)]
    pub(super) fn maybe_as_expression(
        &self,
        value: Option<impl Into<StringOrNumberOrBoolOrRcNode>>,
    ) -> Option<Id<Node>> {
        value.map(|value| self.as_expression(value))
    }

    pub(super) fn as_token(&self, value: SyntaxKindOrRcNode) -> Id<Node> {
        match value {
            SyntaxKindOrRcNode::SyntaxKind(value) => self.create_token(value),
            SyntaxKindOrRcNode::RcNode(value) => value,
        }
    }

    pub(super) fn as_embedded_statement(&self, statement: Option<Id<Node>>) -> Option<Id<Node>> {
        if false {
            unimplemented!()
        } else {
            statement
        }
    }
}

pub enum VecNodeOrModifierFlags {
    VecNode(Vec<Id<Node>>),
    ModifierFlags(ModifierFlags),
}

impl From<Vec<Id<Node>>> for VecNodeOrModifierFlags {
    fn from(value: Vec<Id<Node>>) -> Self {
        Self::VecNode(value)
    }
}

impl From<ModifierFlags> for VecNodeOrModifierFlags {
    fn from(value: ModifierFlags) -> Self {
        Self::ModifierFlags(value)
    }
}

pub enum SyntaxKindOrRcNode {
    SyntaxKind(SyntaxKind),
    RcNode(Id<Node>),
}

impl From<SyntaxKind> for SyntaxKindOrRcNode {
    fn from(value: SyntaxKind) -> Self {
        Self::SyntaxKind(value)
    }
}

impl From<Id<Node>> for SyntaxKindOrRcNode {
    fn from(value: Id<Node>) -> Self {
        Self::RcNode(value)
    }
}

pub(super) fn update_without_original(
    updated: Id<Node>,
    original: Id<Node>,
    arena: &impl HasArena,
) -> Id<Node> {
    if updated != original {
        set_text_range(&*updated.ref_(arena), Some(&*original.ref_(arena)));
    }
    updated
}

pub(super) fn update_with_original(
    updated: Id<Node>,
    original: Id<Node>,
    arena: &impl HasArena,
) -> Id<Node> {
    if updated != original {
        set_original_node(updated, Some(original), arena);
        set_text_range(&*updated.ref_(arena), Some(&*original.ref_(arena)));
    }
    updated
}

pub(super) fn get_default_tag_name_for_kind(kind: SyntaxKind) -> &'static str {
    match kind {
        SyntaxKind::JSDocTypeTag => "type",
        SyntaxKind::JSDocReturnTag => "returns",
        SyntaxKind::JSDocThisTag => "this",
        SyntaxKind::JSDocEnumTag => "enum",
        SyntaxKind::JSDocAuthorTag => "author",
        SyntaxKind::JSDocClassTag => "class",
        SyntaxKind::JSDocPublicTag => "public",
        SyntaxKind::JSDocPrivateTag => "private",
        SyntaxKind::JSDocProtectedTag => "protected",
        SyntaxKind::JSDocReadonlyTag => "readonly",
        SyntaxKind::JSDocOverrideTag => "override",
        SyntaxKind::JSDocTemplateTag => "template",
        SyntaxKind::JSDocTypedefTag => "typedef",
        SyntaxKind::JSDocParameterTag => "parameter",
        SyntaxKind::JSDocPropertyTag => "prop",
        SyntaxKind::JSDocCallbackTag => "callback",
        SyntaxKind::JSDocAugmentsTag => "augments",
        SyntaxKind::JSDocImplementsTag => "implements",
        _ => Debug_.fail(Some(&format!(
            "Unsupported kind: {}",
            Debug_.format_syntax_kind(Some(kind))
        ))),
    }
}

thread_local! {
    pub(super) static raw_text_scanner: RefCell<Option<Scanner>> = RefCell::new(None);
}

pub(super) enum CookedText {
    InvalidValue,
    String(String),
}

pub(super) fn get_cooked_text(
    kind: SyntaxKind, /*TemplateLiteralToken["kind"]*/
    raw_text: &str,
) -> CookedText {
    raw_text_scanner.with(|raw_text_scanner_| {
        let mut raw_text_scanner_ref = raw_text_scanner_.borrow_mut();
        if raw_text_scanner_ref.is_none() {
            *raw_text_scanner_ref = Some(create_scanner(
                ScriptTarget::Latest,
                false,
                Some(LanguageVariant::Standard),
                None,
                None,
                None,
                None,
            ));
        }
        let raw_text_scanner_ref = raw_text_scanner_ref.as_mut().unwrap();
        match kind {
            SyntaxKind::NoSubstitutionTemplateLiteral => {
                let text = format!("`{}`", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateHead => {
                let text = format!("`{}${{", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateMiddle => {
                let text = format!("}}{}${{", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateTail => {
                let text = format!("}}{}`", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            _ => panic!("Unexpected kind"),
        }

        let mut token = raw_text_scanner_ref.scan(None);
        if token == SyntaxKind::CloseBraceToken {
            token = raw_text_scanner_ref.re_scan_template_token(None, false);
        }

        if raw_text_scanner_ref.is_unterminated() {
            raw_text_scanner_ref.set_text(None, None, None, None);
            return CookedText::InvalidValue;
        }

        let mut token_value: Option<String> = None;
        match token {
            SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::TemplateMiddle
            | SyntaxKind::TemplateTail => {
                token_value = Some(raw_text_scanner_ref.get_token_value().clone());
            }
            _ => (),
        }

        if token_value.is_none() || raw_text_scanner_ref.scan(None) != SyntaxKind::EndOfFileToken {
            raw_text_scanner_ref.set_text(None, None, None, None);
            return CookedText::InvalidValue;
        }
        let token_value = token_value.unwrap();

        raw_text_scanner_ref.set_text(None, None, None, None);
        CookedText::String(token_value)
    })
}

pub(super) fn propagate_identifier_name_flags(
    node: Id<Node>, /*Identifier*/
    arena: &impl HasArena,
) -> TransformFlags {
    propagate_child_flags(Some(node), arena) & !TransformFlags::ContainsPossibleTopLevelAwait
}

pub(super) fn propagate_property_name_flags_of_child(
    node: &Node, /*PropertyName*/
    transform_flags: TransformFlags,
) -> TransformFlags {
    transform_flags | (node.transform_flags() & TransformFlags::PropertyNamePropagatingFlags)
}

pub(super) fn propagate_child_flags(
    child: Option<Id<Node>>,
    arena: &impl HasArena,
) -> TransformFlags {
    let Some(child) = child else {
        return TransformFlags::None;
    };
    let child_flags = child.ref_(arena).transform_flags()
        & !get_transform_flags_subtree_exclusions(child.ref_(arena).kind());
    if is_named_declaration(&child.ref_(arena))
        && is_property_name(&child.ref_(arena).as_named_declaration().name().ref_(arena))
    {
        propagate_property_name_flags_of_child(
            &child.ref_(arena).as_named_declaration().name().ref_(arena),
            child_flags,
        )
    } else {
        child_flags
    }
}

pub(super) fn propagate_children_flags(children: Option<&NodeArray>) -> TransformFlags {
    children.map_or(TransformFlags::None, |children| {
        children.maybe_transform_flags().unwrap()
    })
}

pub(super) fn aggregate_children_flags(children: Id<NodeArray>, arena: &impl HasArena) {
    let mut subtree_flags = TransformFlags::None;
    for &child in children.ref_(arena).iter() {
        subtree_flags |= propagate_child_flags(Some(child), arena);
    }
    children
        .ref_(arena)
        .set_transform_flags(Some(subtree_flags));
}

pub(crate) fn get_transform_flags_subtree_exclusions(kind: SyntaxKind) -> TransformFlags {
    if kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode {
        return TransformFlags::TypeExcludes;
    }

    match kind {
        SyntaxKind::CallExpression
        | SyntaxKind::NewExpression
        | SyntaxKind::ArrayLiteralExpression => TransformFlags::ArrayLiteralOrCallOrNewExcludes,
        SyntaxKind::ModuleDeclaration => TransformFlags::ModuleExcludes,
        SyntaxKind::Parameter => TransformFlags::ParameterExcludes,
        SyntaxKind::ArrowFunction => TransformFlags::ArrowFunctionExcludes,
        SyntaxKind::FunctionExpression | SyntaxKind::FunctionDeclaration => {
            TransformFlags::FunctionExcludes
        }
        SyntaxKind::VariableDeclarationList => TransformFlags::VariableDeclarationListExcludes,
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => TransformFlags::ClassExcludes,
        SyntaxKind::Constructor => TransformFlags::ConstructorExcludes,
        SyntaxKind::PropertyDeclaration => TransformFlags::PropertyExcludes,
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            TransformFlags::MethodOrAccessorExcludes
        }
        SyntaxKind::AnyKeyword
        | SyntaxKind::NumberKeyword
        | SyntaxKind::BigIntKeyword
        | SyntaxKind::NeverKeyword
        | SyntaxKind::StringKeyword
        | SyntaxKind::ObjectKeyword
        | SyntaxKind::BooleanKeyword
        | SyntaxKind::SymbolKeyword
        | SyntaxKind::VoidKeyword
        | SyntaxKind::TypeParameter
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration => TransformFlags::TypeExcludes,
        SyntaxKind::ObjectLiteralExpression => TransformFlags::ObjectLiteralExcludes,
        SyntaxKind::CatchClause => TransformFlags::CatchClauseExcludes,
        SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
            TransformFlags::BindingPatternExcludes
        }
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::AsExpression
        | SyntaxKind::PartiallyEmittedExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::SuperKeyword => TransformFlags::OuterExpressionExcludes,
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
            TransformFlags::PropertyAccessExcludes
        }
        _ => TransformFlags::NodeExcludes,
    }
}

pub(super) fn with_base_factory_static<TReturn>(
    mut callback: impl FnMut(&BaseNodeFactoryConcrete) -> TReturn,
    arena: &impl HasArena,
) -> TReturn {
    thread_local! {
        static PER_ARENA: RefCell<HashMap<*const AllArenas, BaseNodeFactoryConcrete>> = RefCell::new(HashMap::new());
    }

    PER_ARENA.with(|per_arena| {
        let mut per_arena = per_arena.borrow_mut();
        let arena_ptr: *const AllArenas = arena.arena();
        callback(
            per_arena
                .entry(arena_ptr)
                .or_insert_with(|| create_base_node_factory(arena)),
        )
    })
}

pub(super) fn make_synthetic_ref(node: &impl NodeInterface) {
    node.set_flags(node.flags() | NodeFlags::Synthesized);
}

pub(super) fn make_synthetic(node: BaseNode) -> BaseNode {
    make_synthetic_ref(&node);
    node
}

pub fn get_synthetic_factory(arena: &impl HasArena) -> Id<Box<dyn BaseNodeFactory>> {
    per_arena!(
        Box<dyn BaseNodeFactory>,
        arena,
        arena.alloc_base_node_factory(Box::new(BaseNodeFactorySynthetic::new(arena)))
    )
}

#[derive(Debug)]
pub struct BaseNodeFactorySynthetic {
    arena: *const AllArenas,
}

impl BaseNodeFactorySynthetic {
    pub fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl BaseNodeFactory for BaseNodeFactorySynthetic {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(with_base_factory_static(
            |base_factory| base_factory.create_base_source_file_node(kind),
            self,
        ))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(with_base_factory_static(
            |base_factory| base_factory.create_base_identifier_node(kind),
            self,
        ))
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(with_base_factory_static(
            |base_factory| base_factory.create_base_private_identifier_node(kind),
            self,
        ))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(with_base_factory_static(
            |base_factory| base_factory.create_base_token_node(kind),
            self,
        ))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(with_base_factory_static(
            |base_factory| base_factory.create_base_node(kind),
            self,
        ))
    }

    fn update_cloned_node(&self, node: &BaseNode) {
        make_synthetic_ref(node);
    }
}

impl_has_arena!(BaseNodeFactorySynthetic);

pub fn get_factory_id(arena: &impl HasArena) -> Id<NodeFactory> {
    per_arena!(
        NodeFactory,
        arena,
        create_node_factory(
            NodeFactoryFlags::NoIndentationOnFreshPropertyAccess,
            get_synthetic_factory(arena),
            arena,
        )
    )
}

pub fn get_factory(arena: &impl HasArena) -> debug_cell::Ref<'_, NodeFactory> {
    get_factory_id(arena).ref_(arena)
}

pub enum PseudoBigIntOrString {
    PseudoBigInt(PseudoBigInt),
    String(String),
}

impl From<PseudoBigInt> for PseudoBigIntOrString {
    fn from(pseudo_big_int: PseudoBigInt) -> Self {
        PseudoBigIntOrString::PseudoBigInt(pseudo_big_int)
    }
}

impl From<String> for PseudoBigIntOrString {
    fn from(string: String) -> Self {
        PseudoBigIntOrString::String(string)
    }
}

pub fn create_unparsed_source_file(
    _text_or_input_files: impl Into<StringOrRcNode>,
    _map_path_or_type: Option<&str>,
    _map_text_or_strip_internal: Option<impl Into<StringOrBool>>,
) -> Id<Node /*UnparsedSource*/> {
    unimplemented!()
}

pub fn create_input_files(
    javascript_text_or_read_file_text: impl Into<StringOrReadFileCallback>,
    declaration_text_or_javascript_path: String,
    javascript_map_path: Option<String>,
    javascript_map_text_or_declaration_path: Option<String>,
    declaration_map_path: Option<String>,
    declaration_map_text_or_build_info_path: Option<String>,
    javascript_path: Option<String>,
    declaration_path: Option<String>,
    build_info_path: Option<String>,
    build_info: Option<Id<BuildInfo>>,
    old_file_of_current_emit: Option<bool>,
    arena: &impl HasArena,
) -> Id<Node /*InputFiles*/> {
    let mut node: InputFiles = get_parse_node_factory(arena)
        .create_input_files_raw()
        .into();
    match javascript_text_or_read_file_text.into() {
        StringOrReadFileCallback::ReadFileCallback(javascript_text_or_read_file_text) => {
            node.initialize_with_read_file_callback(
                javascript_text_or_read_file_text,
                declaration_text_or_javascript_path,
                javascript_map_path,
                javascript_map_text_or_declaration_path,
                declaration_map_path,
                declaration_map_text_or_build_info_path,
            );
        }
        StringOrReadFileCallback::String(javascript_text_or_read_file_text) => {
            node.initialize_with_string(
                javascript_text_or_read_file_text,
                javascript_map_path,
                javascript_map_text_or_declaration_path,
                declaration_text_or_javascript_path,
                declaration_map_path,
                declaration_map_text_or_build_info_path,
                javascript_path,
                declaration_path,
                build_info_path,
                build_info,
                old_file_of_current_emit,
            );
        }
    }
    arena.alloc_node(node.into())
}

pub trait ReadFileCallback: fmt::Debug {
    fn call(&self, path: &str) -> Option<String>;
}

pub enum StringOrReadFileCallback {
    String(String),
    ReadFileCallback(Id<Box<dyn ReadFileCallback>>),
}

impl From<String> for StringOrReadFileCallback {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Id<Box<dyn ReadFileCallback>>> for StringOrReadFileCallback {
    fn from(value: Id<Box<dyn ReadFileCallback>>) -> Self {
        Self::ReadFileCallback(value)
    }
}

pub fn set_original_node(
    node: Id<Node>,
    original: Option<Id<Node>>,
    arena: &impl HasArena,
) -> Id<Node> {
    node.ref_(arena).set_original(original);
    if let Some(original) = original {
        let emit_node = original.ref_(arena).maybe_emit_node();
        if let Some(emit_node) = emit_node {
            let node_emit_node = {
                if node.ref_(arena).maybe_emit_node().is_none() {
                    node.ref_(arena)
                        .set_emit_node(Some(arena.alloc_emit_node(Default::default())));
                }
                node.ref_(arena).maybe_emit_node().unwrap()
            };
            merge_emit_node(emit_node, node_emit_node, arena);
            // node.set_emit_node(node_emit_node);
        }
    }
    node
}

pub(super) fn merge_emit_node(
    source_emit_node: Id<EmitNode>,
    dest_emit_node: /*Option<*/ Id<EmitNode>, /*>*/
    arena: &impl HasArena,
) /*-> EmitNode*/
{
    let flags = source_emit_node.ref_(arena).flags;
    let leading_comments = source_emit_node.ref_(arena).leading_comments.clone();
    let trailing_comments = source_emit_node.ref_(arena).trailing_comments.clone();
    let comment_range = source_emit_node.ref_(arena).comment_range.clone();
    let source_map_range = source_emit_node.ref_(arena).source_map_range;
    let token_source_map_ranges = source_emit_node.ref_(arena).token_source_map_ranges.clone();
    let constant_value = source_emit_node.ref_(arena).constant_value.clone();
    let helpers = source_emit_node.ref_(arena).helpers.clone();
    let starts_on_new_line = source_emit_node.ref_(arena).starts_on_new_line;
    if let Some(mut leading_comments) = leading_comments {
        add_range(
            &mut leading_comments,
            dest_emit_node.ref_(arena).leading_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.ref_mut(arena).leading_comments = Some(leading_comments);
    }
    if let Some(mut trailing_comments) = trailing_comments {
        add_range(
            &mut trailing_comments,
            dest_emit_node.ref_(arena).trailing_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.ref_mut(arena).trailing_comments = Some(trailing_comments);
    }
    // TODO: should this technically also check "truthiness" of flags?
    if let Some(flags) = flags {
        dest_emit_node.ref_mut(arena).flags = Some(flags & !EmitFlags::Immutable);
    }
    if comment_range.is_some() {
        dest_emit_node.ref_mut(arena).comment_range = comment_range;
    }
    if source_map_range.is_some() {
        dest_emit_node.ref_mut(arena).source_map_range = source_map_range;
    }
    if let Some(token_source_map_ranges) = token_source_map_ranges {
        let token_source_map_ranges = merge_token_source_map_ranges(
            &token_source_map_ranges,
            dest_emit_node.ref_(arena).token_source_map_ranges.as_ref(),
        );
        dest_emit_node.ref_mut(arena).token_source_map_ranges = Some(token_source_map_ranges);
    }
    if constant_value.is_some() {
        dest_emit_node.ref_mut(arena).constant_value = constant_value;
    }
    if let Some(helpers) = helpers {
        let mut dest_emit_node_helpers = dest_emit_node.ref_(arena).helpers.clone();
        for helper in helpers {
            dest_emit_node_helpers =
                Some(maybe_append_if_unique_eq(dest_emit_node_helpers, &helper));
        }
        dest_emit_node.ref_mut(arena).helpers = dest_emit_node_helpers;
    }
    if starts_on_new_line.is_some() {
        dest_emit_node.ref_mut(arena).starts_on_new_line = starts_on_new_line;
    }
    // return destEmitNode
}

pub(super) fn merge_token_source_map_ranges(
    source_ranges: &HashMap<SyntaxKind, Option<Id<SourceMapRange>>>,
    dest_ranges: Option<&HashMap<SyntaxKind, Option<Id<SourceMapRange>>>>,
) -> HashMap<SyntaxKind, Option<Id<SourceMapRange>>> {
    let mut dest_ranges =
        dest_ranges.map_or_else(|| HashMap::new(), |dest_ranges| dest_ranges.clone());
    for (key, value) in source_ranges {
        dest_ranges.insert(*key, value.clone());
    }
    dest_ranges
}
