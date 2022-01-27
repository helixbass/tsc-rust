use regex::Regex;
use serde_json;
use std::borrow::Borrow;
use std::cmp;
use std::collections::HashMap;
use std::ops::BitOrAssign;
use std::ptr;
use std::rc::Rc;

use crate::{
    combine_paths, contains, create_compiler_diagnostic, every, find, flat_map,
    get_assignment_declaration_kind, get_directory_path, get_effective_modifier_flags,
    get_effective_modifier_flags_always_include_jsdoc,
    get_element_or_property_access_argument_expression_or_name, get_emit_script_target,
    get_jsdoc_comments_and_tags, has_syntactic_modifier, is_access_expression, is_arrow_function,
    is_bindable_static_element_access_expression, is_binding_element, is_class_expression,
    is_class_static_block_declaration, is_function_expression, is_identifier, is_jsdoc,
    is_jsdoc_augments_tag, is_jsdoc_implements_tag, is_jsdoc_parameter_tag, is_jsdoc_template_tag,
    is_jsdoc_type_tag, is_omitted_expression, is_rooted_disk_path, is_variable_statement,
    normalize_path, path_is_relative, set_localized_diagnostic_messages, set_ui_locale,
    skip_outer_expressions, some, AssignmentDeclarationKind, CharacterCodes, CompilerOptions,
    Debug_, Diagnostics, Expression, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags,
    NodeInterface, OuterExpressionKinds, Push, ScriptTarget, Statement, Symbol, SymbolInterface,
    SyntaxKind, System, TextChangeRange, TextRange, TextSpan, __String, compare_diagnostics,
    is_block, is_module_block, is_source_file, sort_and_deduplicate, Diagnostic, SortedArray,
};

pub fn is_external_module_name_relative(module_name: &str) -> bool {
    path_is_relative(module_name) || is_rooted_disk_path(module_name)
}

pub fn sort_and_deduplicate_diagnostics(
    diagnostics: &[Rc<Diagnostic>],
) -> SortedArray<Rc<Diagnostic>> {
    sort_and_deduplicate(
        diagnostics,
        &|a, b| compare_diagnostics(&**a, &**b),
        Option::<&fn(&Rc<Diagnostic>, &Rc<Diagnostic>) -> bool>::None,
    )
}

pub fn get_default_lib_file_name(options: &CompilerOptions) -> &'static str {
    match get_emit_script_target(options) {
        ScriptTarget::ESNext => "lib.esnext.full.d.ts",
        ScriptTarget::ES2021 => "lib.es2021.full.d.ts",
        ScriptTarget::ES2020 => "lib.es2020.full.d.ts",
        ScriptTarget::ES2019 => "lib.es2019.full.d.ts",
        ScriptTarget::ES2018 => "lib.es2018.full.d.ts",
        ScriptTarget::ES2017 => "lib.es2017.full.d.ts",
        ScriptTarget::ES2016 => "lib.es2016.full.d.ts",
        ScriptTarget::ES2015 => "lib.es6.d.ts",
        _ => "lib.d.ts",
    }
}

pub fn text_span_end(span: &TextSpan) -> isize {
    span.start + span.length
}

pub fn text_span_is_empty(span: &TextSpan) -> bool {
    span.length == 0
}

pub fn text_span_contains_position(span: &TextSpan, position: isize) -> bool {
    position >= span.start && position < text_span_end(span)
}

pub(crate) fn text_range_contains_position_inclusive<TSpan: TextRange>(
    span: &TSpan,
    position: isize,
) -> bool {
    position >= span.pos() && position <= span.end()
}

pub fn text_span_contains_text_span(span: &TextSpan, other: &TextSpan) -> bool {
    other.start >= span.start && text_span_end(other) <= text_span_end(span)
}

pub fn text_span_overlaps_with(span: &TextSpan, other: &TextSpan) -> bool {
    text_span_overlap(span, other).is_some()
}

pub fn text_span_overlap(span1: &TextSpan, span2: &TextSpan) -> Option<TextSpan> {
    let overlap = text_span_intersection(span1, span2);
    match overlap {
        Some(overlap) if overlap.length == 0 => None,
        _ => overlap,
    }
}

pub fn text_span_intersects_with_text_span(span: &TextSpan, other: &TextSpan) -> bool {
    decoded_text_span_intersects_with(span.start, span.length, other.start, other.length)
}

pub fn text_span_intersects_with(span: &TextSpan, start: isize, length: isize) -> bool {
    decoded_text_span_intersects_with(span.start, span.length, start, length)
}

pub fn decoded_text_span_intersects_with(
    start1: isize,
    length1: isize,
    start2: isize,
    length2: isize,
) -> bool {
    let end1 = start1 + length1;
    let end2 = start2 + length2;
    start2 <= end1 && end2 >= start1
}

pub fn text_span_intersects_with_position(span: &TextSpan, position: isize) -> bool {
    position <= text_span_end(span) && position >= span.start
}

pub fn text_span_intersection(span1: &TextSpan, span2: &TextSpan) -> Option<TextSpan> {
    let start = cmp::max(span1.start, span2.start);
    let end = cmp::min(text_span_end(span1), text_span_end(span2));
    if start <= end {
        Some(create_text_span_from_bounds(start, end))
    } else {
        None
    }
}

fn create_text_span(start: isize, length: isize) -> TextSpan {
    if start < 0 {
        panic!("start < 0");
    }
    if length < 0 {
        panic!("length < 0");
    }

    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: isize, end: isize) -> TextSpan {
    create_text_span(start, end - start)
}

pub fn text_change_range_new_span(range: &TextChangeRange) -> TextSpan {
    create_text_span(range.span.start, range.new_length)
}

pub fn text_change_range_is_unchanged(range: &TextChangeRange) -> bool {
    text_span_is_empty(&range.span) && range.new_length == 0
}

pub fn create_text_change_range(span: TextSpan, new_length: isize) -> TextChangeRange {
    if new_length < 0 {
        panic!("newLength < 0");
    }

    TextChangeRange { span, new_length }
}

lazy_static! {
    pub static ref unchanged_text_change_range: TextChangeRange =
        create_text_change_range(create_text_span(0, 0), 0);
}

pub fn collapse_text_change_ranges_across_multiple_versions(
    changes: &[TextChangeRange],
) -> TextChangeRange {
    if changes.is_empty() {
        let unchanged_text_change_range_ref: &TextChangeRange = &unchanged_text_change_range;
        return *unchanged_text_change_range_ref;
    }

    if changes.len() == 1 {
        return changes[0];
    }

    let change0 = changes[0];

    let mut old_start_n = change0.span.start;
    let mut old_end_n = text_span_end(&change0.span);
    let mut new_end_n = old_start_n + change0.new_length;

    for next_change in changes.iter().skip(1) {
        let old_start_1 = old_start_n;
        let old_end_1 = old_end_n;
        let new_end_1 = new_end_n;

        let old_start_2 = next_change.span.start;
        let old_end_2 = text_span_end(&next_change.span);
        let new_end_2 = old_start_2 + next_change.new_length;

        old_start_n = cmp::min(old_start_1, old_start_2);
        old_end_n = cmp::max(old_end_1, old_end_1 + (old_end_2 - new_end_1));
        new_end_n = cmp::max(new_end_2, old_end_2 + (new_end_1 - old_end_2));
    }

    create_text_change_range(
        create_text_span_from_bounds(old_start_n, old_end_n),
        new_end_n - old_start_n,
    )
}

pub fn get_type_parameter_owner(d: &Node /*Declaration*/) -> Option<Rc<Node>> {
    if
    /*d && */
    d.kind() == SyntaxKind::TypeParameter {
        let mut current = Some(d.node_wrapper());
        while current.is_some() {
            let current_present = current.clone().unwrap();
            if is_function_like(current.clone())
                || is_class_like(&current_present)
                || current_present.kind() == SyntaxKind::InterfaceDeclaration
            {
                return current;
            }
            current = current_present.maybe_parent();
        }
    }
    None
}

// export type ParameterPropertyDeclaration = ParameterDeclaration & { parent: ConstructorDeclaration, name: Identifier };
pub fn is_parameter_property_declaration(node: &Node, parent: &Node) -> bool {
    has_syntactic_modifier(node, ModifierFlags::ParameterPropertyModifier)
        && parent.kind() == SyntaxKind::Constructor
}

pub fn is_empty_binding_pattern(node: &Node /*BindingName*/) -> bool {
    if is_binding_pattern(node) {
        return every(node.as_has_elements().elements(), |element, _| {
            is_empty_binding_element(element)
        });
    }
    false
}

pub fn is_empty_binding_element(node: &Node /*BindingElement*/) -> bool {
    if is_omitted_expression(node) {
        return true;
    }
    is_empty_binding_pattern(&node.as_named_declaration().name())
}

pub fn walk_up_binding_elements_and_patterns(binding: &Node /*BindingElement*/) -> Rc<Node> /*VariableDeclaration | ParameterDeclaration*/
{
    let mut node = binding.parent();
    while is_binding_element(&*node.parent()) {
        node = node.parent().parent();
    }
    node.parent()
}

fn get_combined_flags<
    TNode: NodeInterface,
    TFlags: BitOrAssign,
    TGetFlags: FnMut(&Node) -> TFlags,
>(
    node: &TNode,
    mut get_flags: TGetFlags,
) -> TFlags {
    let mut node = Some(node.node_wrapper());
    if is_binding_element(&**node.as_ref().unwrap()) {
        node = Some(walk_up_binding_elements_and_patterns(
            node.as_ref().unwrap(),
        ));
    }
    let mut flags = get_flags(node.as_ref().unwrap());
    if node.as_ref().unwrap().kind() == SyntaxKind::VariableDeclaration {
        node = node.as_ref().unwrap().maybe_parent();
    }
    if let Some(node_present) = node.as_ref() {
        if node_present.kind() == SyntaxKind::VariableDeclarationList {
            flags |= get_flags(node_present);
            node = node_present.maybe_parent();
        }
    }
    if let Some(node) = node {
        if node.kind() == SyntaxKind::VariableStatement {
            flags |= get_flags(&*node);
        }
    }
    flags
}

pub fn get_combined_modifier_flags<TNode: NodeInterface>(
    node: &TNode, /*Declaration*/
) -> ModifierFlags {
    get_combined_flags(node, get_effective_modifier_flags)
}

pub(crate) fn get_combined_node_flags_always_include_jsdoc<TNode: NodeInterface>(
    node: &TNode, /*Declaration*/
) -> ModifierFlags {
    get_combined_flags(node, get_effective_modifier_flags_always_include_jsdoc)
}

pub fn get_combined_node_flags<TNode: NodeInterface>(node: &TNode) -> NodeFlags {
    get_combined_flags(node, |n| n.flags())
}

lazy_static! {
    pub static ref supported_locale_directories: Vec<&'static str> = vec![
        "cs", "de", "es", "fr", "it", "ja", "ko", "pl", "pt-br", "ru", "tr", "zh-cn", "zh-tw",
    ];
}

pub fn validate_locale_and_set_language<TSys: System>(
    locale: &str,
    sys: &TSys,
    mut errors: Option<&mut Push<Diagnostic>>,
) {
    let lower_case_locale = locale.to_lowercase();
    lazy_static! {
        static ref regex: Regex = Regex::new(r"^([a-z]+)([_\-]([a-z]+))?$").unwrap();
    }
    let match_result = regex.captures(&lower_case_locale);

    if match_result.is_none() {
        if let Some(errors) = errors {
            errors.push(create_compiler_diagnostic(&Diagnostics::Locale_must_be_of_the_form_language_or_language_territory_For_example_0_or_1, Some(vec!["en".to_owned(), "ja-jp".to_owned()])).into());
        }
        return;
    }
    let match_result = match_result.unwrap();

    let language = &match_result[1];
    let territory = &match_result[3];

    let lower_case_locale_str: &str = &lower_case_locale;
    if contains(Some(&supported_locale_directories), &lower_case_locale_str)
        && !try_set_language_and_territory(sys, language, Some(territory), &mut errors)
    {
        try_set_language_and_territory(sys, language, None, &mut errors);
    }

    set_ui_locale(Some(locale.to_owned()));
}

fn try_set_language_and_territory<TSys: System>(
    sys: &TSys,
    language: &str,
    territory: Option<&str>,
    errors: &mut Option<&mut Push<Diagnostic>>,
) -> bool {
    let compiler_file_path = normalize_path(&sys.get_executing_file_path());
    let containing_directory_path = get_directory_path(&compiler_file_path);

    let mut file_path = combine_paths(&containing_directory_path, &vec![Some(language)]);

    if let Some(territory) = territory {
        file_path = format!("{}-{}", file_path, territory);
    }

    file_path = sys.resolve_path(&combine_paths(
        &file_path,
        &vec![Some("diagnosticMessages.generated.json")],
    ));

    if !sys.file_exists(&file_path) {
        return false;
    }

    let mut file_contents: Option<String> = Some("".to_owned());
    file_contents = sys.read_file(&file_path);
    if file_contents.is_none() {
        if let Some(errors) = errors {
            errors.push(
                create_compiler_diagnostic(
                    &Diagnostics::Unable_to_open_file_0,
                    Some(vec![file_path]),
                )
                .into(),
            );
        }
        return false;
    }
    let file_contents = file_contents.unwrap();
    let parsed_file_contents: serde_json::Result<HashMap<String, String>> =
        serde_json::from_str(&file_contents);
    if parsed_file_contents.is_err() {
        if let Some(errors) = errors {
            errors.push(
                create_compiler_diagnostic(
                    &Diagnostics::Corrupted_locale_file_0,
                    Some(vec![file_path]),
                )
                .into(),
            );
        }
        return false;
    }
    let parsed_file_contents = parsed_file_contents.unwrap();
    set_localized_diagnostic_messages(Some(parsed_file_contents));

    true
}

pub fn get_original_node<TNode: Borrow<Node>, TNodeTest: FnOnce(Option<Rc<Node>>) -> bool>(
    node: Option<TNode>,
    node_test: Option<TNodeTest>,
) -> Option<Rc<Node>> {
    let mut node = node.map(|node| node.borrow().node_wrapper());

    if let Some(node_present) = node.clone() {
        while let Some(node_original) = node_present.maybe_original() {
            node = Some(node_original);
        }
    }

    if match node_test {
        None => true,
        Some(node_test) => node_test(node.clone()),
    } {
        node
    } else {
        None
    }
}

pub enum FindAncestorCallbackReturn {
    Bool(bool),
    Quit,
}

impl From<bool> for FindAncestorCallbackReturn {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

pub fn find_ancestor<
    TNodeRef: Borrow<Node>,
    TCallbackReturn: Into<FindAncestorCallbackReturn>,
    TCallback: FnMut(&Node) -> TCallbackReturn,
>(
    node: Option<TNodeRef>,
    mut callback: TCallback,
) -> Option<Rc<Node>> {
    let mut node = node.map(|node| node.borrow().node_wrapper());
    while let Some(rc_node_ref) = node.as_ref() {
        let result = callback(&**rc_node_ref).into();
        match result {
            FindAncestorCallbackReturn::Quit => {
                return None;
            }
            FindAncestorCallbackReturn::Bool(result) if result => {
                return node;
            }
            _ => (),
        }
        node = rc_node_ref.maybe_parent();
    }
    None
}

pub fn is_parse_tree_node(node: &Node) -> bool {
    !node.flags().intersects(NodeFlags::Synthesized)
}

pub fn get_parse_tree_node<TNode: Borrow<Node>, TNodeTest: FnOnce(Option<Rc<Node>>) -> bool>(
    node: Option<TNode>,
    node_test: Option<TNodeTest>,
) -> Option<Rc<Node>> {
    let node = node.map(|node| node.borrow().node_wrapper());

    match node {
        None => {
            return node;
        }
        Some(node) if is_parse_tree_node(&node) => {
            return Some(node);
        }
        _ => (),
    }
    let node = node.unwrap();

    let mut node = node.maybe_original();
    while let Some(node_present) = node.clone() {
        if is_parse_tree_node(&node_present) {
            return if match node_test {
                None => true,
                Some(node_test) => node_test(node.clone()),
            } {
                node
            } else {
                None
            };
        }
        node = node_present.maybe_original();
    }
    None
}

pub fn escape_leading_underscores(identifier: &str) -> __String {
    let identifier_chars: Vec<char> = identifier.chars().collect();
    __String::new(
        if identifier_chars.len() >= 2
            && identifier_chars[0] == CharacterCodes::underscore
            && identifier_chars[1] == CharacterCodes::underscore
        {
            format!("_{}", identifier)
        } else {
            identifier.to_string()
        },
    )
}

pub fn unescape_leading_underscores(identifier: &__String) -> String {
    let mut chars = identifier.chars();
    if
    /*chars.count() >= 3 &&*/
    matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
    {
        identifier.chars().skip(1).collect()
    } else {
        identifier.chars().collect()
    }
}

pub fn id_text(identifier_or_private_name: &Node, /*Identifier | PrivateIdentifier*/) -> String {
    unescape_leading_underscores(&identifier_or_private_name.as_identifier().escaped_text)
}

pub fn symbol_name(symbol: &Symbol) -> String {
    match symbol
        .maybe_value_declaration()
        .map(|weak| weak.upgrade().unwrap())
    {
        Some(symbol_value_declaration)
            if is_private_identifier_class_element_declaration(&symbol_value_declaration) =>
        {
            return id_text(&symbol_value_declaration.as_named_declaration().name());
        }
        _ => (),
    }
    unescape_leading_underscores(&symbol.escaped_name())
}

fn name_for_nameless_jsdoc_typedef(
    declaration: &Node, /*JSDocTypedefTag | JSDocEnumTag*/
) -> Option<Rc<Node /*Identifier | PrivateIdentifier*/>> {
    let host_node = declaration.parent().maybe_parent();
    if host_node.is_none() {
        return None;
    }
    let host_node = host_node.unwrap();
    if is_declaration(&host_node) {
        return get_declaration_identifier(&host_node);
    }
    match &*host_node {
        Node::Statement(Statement::VariableStatement(host_node)) => {
            if
            /*hostNode.declarationList &&*/
            !host_node
                .declaration_list
                .as_variable_declaration_list()
                .declarations
                .is_empty()
            {
                return get_declaration_identifier(
                    &host_node
                        .declaration_list
                        .as_variable_declaration_list()
                        .declarations[0],
                );
            }
        }
        Node::Statement(Statement::ExpressionStatement(host_node)) => {
            let mut expr = host_node.expression.clone();
            match &*expr {
                Node::Expression(Expression::BinaryExpression(expr_as_binary_expression))
                    if expr_as_binary_expression.operator_token.kind()
                        == SyntaxKind::EqualsToken =>
                {
                    expr = expr_as_binary_expression.left.clone();
                }
                _ => (),
            }
            match &*expr {
                Node::Expression(Expression::PropertyAccessExpression(expr)) => {
                    return Some(expr.name());
                }
                Node::Expression(Expression::ElementAccessExpression(expr)) => {
                    let arg = expr.argument_expression.clone();
                    if is_identifier(&*arg) {
                        return arg;
                    }
                }
                _ => (),
            }
        }
        Node::Expression(Expression::ParenthesizedExpression(host_node)) => {
            return get_declaration_identifier(&host_node.expression);
        }
        Node::Statement(Statement::LabeledStatement(host_node)) => {
            if is_declaration(&host_node.statement) || is_expression(&host_node.statement) {
                return get_declaration_identifier(&host_node.statement);
            }
        }
    }
    None
}

fn get_declaration_identifier(
    node: &Node, /*Declaration | Expression*/
) -> Option<Rc<Node /*Identifier*/>> {
    let name = get_name_of_declaration(node);
    name.filter(|name| is_identifier(&**name))
}

pub(crate) fn node_has_name(statement: &Node, name: &Node /*Identifier*/) -> bool {
    if is_named_declaration(statement)
        && is_identifier(&*statement.as_named_declaration().name())
        && id_text(&statement.as_named_declaration().name()) == id_text(name)
    {
        return true;
    }
    if is_variable_statement(statement)
        && some(
            Some(
                &statement
                    .as_variable_statement()
                    .declaration_list
                    .as_variable_declaration_list()
                    .declarations,
            ),
            Some(|d: &Rc<Node>| node_has_name(d, name)),
        )
    {
        return true;
    }
    false
}

pub fn get_name_of_jsdoc_typedef(
    declaration: &Node, /*JSDocTypedefTag*/
) -> Option<Rc<Node /*Identifier | PrivateIdentifier*/>> {
    declaration
        .as_jsdoc_typedef_tag()
        .name
        .clone()
        .or_else(|| name_for_nameless_jsdoc_typedef(declaration))
}

pub(crate) fn is_named_declaration(node: &Node) -> bool {
    node.maybe_as_named_declaration()
        .map_or(false, |node| node.maybe_name().is_some())
}

fn get_non_assigned_name_of_declaration(
    declaration: &Node, /*Declaration | Expression*/
) -> Option<Rc<Node /*DeclarationName*/>> {
    match declaration.kind() {
        SyntaxKind::Identifier => {
            return Some(declaration.node_wrapper());
        }
        SyntaxKind::JSDocPropertyTag | SyntaxKind::JSDocParameterTag => {
            let name = &declaration.as_jsdoc_property_like_tag().name;
            if let Node::QualifiedName(name) = &**name {
                return Some(name.right.clone());
            }
        }
        SyntaxKind::CallExpression | SyntaxKind::BinaryExpression => {
            let expr = declaration;
            match get_assignment_declaration_kind(expr) {
                AssignmentDeclarationKind::ExportsProperty
                | AssignmentDeclarationKind::ThisProperty
                | AssignmentDeclarationKind::Property
                | AssignmentDeclarationKind::PrototypeProperty => {
                    return get_element_or_property_access_argument_expression_or_name(
                        &expr.as_binary_expression().left,
                    );
                }
                AssignmentDeclarationKind::ObjectDefinePropertyValue
                | AssignmentDeclarationKind::ObjectDefinePropertyExports
                | AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
                    return expr.as_call_expression().arguments[1].clone();
                }
                _ => {
                    return None;
                }
            }
        }
        SyntaxKind::JSDocTypedefTag => {
            return get_name_of_jsdoc_typedef(declaration);
        }
        SyntaxKind::JSDocEnumTag => {
            return name_for_nameless_jsdoc_typedef(declaration);
        }
        SyntaxKind::ExportAssignment => {
            let expression = &declaration.as_export_assignment().expression;
            return if is_identifier(&**expression) {
                Some(expression.clone())
            } else {
                None
            };
        }
        SyntaxKind::ElementAccessExpression => {
            let expr = declaration;
            if is_bindable_static_element_access_expression(expr, None) {
                return Some(
                    expr.as_element_access_expression()
                        .argument_expression
                        .clone(),
                );
            }
        }
    }
    declaration.as_named_declaration().maybe_name()
}

pub fn get_name_of_declaration<TNode: Borrow<Node>>(
    declaration: Option<TNode /*Declaration | Expression*/>,
) -> Option<Rc<Node /*DeclarationName*/>> {
    if declaration.is_none() {
        return None;
    }
    let declaration = declaration.unwrap();
    let declaration = declaration.borrow();
    get_non_assigned_name_of_declaration(declaration).or_else(|| {
        if is_function_expression(declaration)
            || is_arrow_function(declaration)
            || is_class_expression(declaration)
        {
            get_assigned_name(declaration)
        } else {
            None
        }
    })
}

pub(crate) fn get_assigned_name(node: &Node) -> Option<Rc<Node /*DeclarationName*/>> {
    let node_parent = node.maybe_parent();
    if node_parent.is_none() {
        return None;
    }
    let node_parent = node_parent.unwrap();
    match &*node_parent {
        Node::PropertyAssignment(_) | Node::BindingElement(_) => {
            return node_parent.as_named_declaration().maybe_name();
        }
        Node::Expression(Expression::BinaryExpression(node_parent)) => {
            if ptr::eq(node, &*node_parent.right) {
                if is_identifier(&*node_parent.left) {
                    return Some(node_parent.left.clone());
                } else if is_access_expression(&*node_parent.left) {
                    return get_element_or_property_access_argument_expression_or_name(
                        &node_parent.left,
                    );
                }
            }
        }
        Node::VariableDeclaration(node_parent) => {
            if matches!(node_parent.maybe_name(), Some(name) if is_identifier(&*name)) {
                return node_parent.maybe_name();
            }
        }
        _ => (),
    }
    None
}

fn get_jsdoc_parameter_tags_worker(
    param: &Node, /*ParameterDeclaration*/
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    let param_as_parameter_declaration = param.as_parameter_declaration();
    /*if param.name {*/
    if is_identifier(&*param_as_parameter_declaration.name()) {
        let name = param_as_parameter_declaration
            .name()
            .as_identifier()
            .escaped_text
            .clone();
        return get_jsdoc_tags_worker(&param.parent(), no_cache)
            .into_iter()
            .filter(|tag| {
                if !is_jsdoc_parameter_tag(&**tag) {
                    return false;
                }
                let tag_as_jsdoc_parameter_tag = tag.as_jsdoc_property_like_tag();
                if !is_identifier(&*tag_as_jsdoc_parameter_tag.name) {
                    return false;
                }
                tag_as_jsdoc_parameter_tag.name.as_identifier().escaped_text == name
            })
            .collect();
    } else {
        let i = param
            .parent()
            .as_signature_declaration()
            .parameters()
            .iter()
            .position(|parameter| ptr::eq(&**parameter, param));
        Debug_.assert(
            i.is_some(),
            Some("Parameters should always be in their parent's parameter lists"),
        );
        let i = i.unwrap();
        let param_tags: Vec<Rc<Node>> = get_jsdoc_tags_worker(&param.parent(), no_cache)
            .into_iter()
            .filter(|tag| is_jsdoc_parameter_tag(&**tag))
            .collect();
        if i < param_tags.len() {
            return vec![param_tags[i].clone()];
        }
    }
    /*}*/
    vec![]
}

pub fn get_jsdoc_parameter_tags(
    param: &Node, /*ParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    get_jsdoc_parameter_tags_worker(param, Some(false))
}

pub(crate) fn get_jsdoc_parameter_tags_no_cache(
    param: &Node, /*ParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    get_jsdoc_parameter_tags_worker(param, Some(true))
}

fn get_jsdoc_type_parameter_tags_worker(
    param: &Node, /*TypeParameterDeclaration*/
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    let name = param
        .as_type_parameter_declaration()
        .name()
        .as_identifier()
        .escaped_text
        .clone();
    get_jsdoc_tags_worker(&param.parent(), no_cache)
        .into_iter()
        .filter(|tag| {
            if !is_jsdoc_template_tag(&**tag) {
                return false;
            }
            let tag_as_jsdoc_template_tag = tag.as_jsdoc_template_tag();
            tag_as_jsdoc_template_tag.type_parameters.iter().any(|tp| {
                tp.as_type_parameter_declaration()
                    .name()
                    .as_identifier()
                    .escaped_text
                    == name
            })
        })
        .collect()
}

pub fn get_jsdoc_type_parameter_tags(
    param: &Node, /*TypeParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    get_jsdoc_type_parameter_tags_worker(param, Some(false))
}

pub(crate) fn get_jsdoc_type_parameter_tags_no_cache(
    param: &Node, /*TypeParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    get_jsdoc_type_parameter_tags_worker(param, Some(true))
}

pub fn has_jsdoc_parameter_tags(
    node: &Node, /*FunctionLikeDeclaration | SignatureDeclaration*/
) -> bool {
    get_first_jsdoc_tag(node, is_jsdoc_parameter_tag, None).is_some()
}

pub fn get_jsdoc_augments_tag(node: &Node) -> Option<Rc<Node /*JSDocAugmentsTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_augments_tag, None)
}

pub fn get_jsdoc_implements_tags(node: &Node) -> Vec<Rc<Node /*JSDocImplementsTag*/>> {
    get_all_jsdoc_tags(node, is_jsdoc_implements_tag)
}

pub fn get_jsdoc_class_tag(node: &Node) -> Option<Rc<Node /*JSDocClassTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_class_tag, None)
}

pub fn get_jsdoc_public_tag(node: &Node) -> Option<Rc<Node /*JSDocPublicTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_public_tag, None)
}

pub(crate) fn get_jsdoc_public_tag_no_cache(node: &Node) -> Option<Rc<Node /*JSDocPublicTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_public_tag, Some(true))
}

pub fn get_jsdoc_private_tag(node: &Node) -> Option<Rc<Node /*JSDocPrivateTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_private_tag, None)
}

pub(crate) fn get_jsdoc_private_tag_no_cache(node: &Node) -> Option<Rc<Node /*JSDocPrivateTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_private_tag, Some(true))
}

pub fn get_jsdoc_protected_tag(node: &Node) -> Option<Rc<Node /*JSDocProtectedTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_protected_tag, None)
}

pub(crate) fn get_jsdoc_protected_tag_no_cache(
    node: &Node,
) -> Option<Rc<Node /*JSDocProtectedTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_protected_tag, Some(true))
}

pub fn get_jsdoc_readonly_tag(node: &Node) -> Option<Rc<Node /*JSDocReadonlyTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_readonly_tag, None)
}

pub(crate) fn get_jsdoc_readonly_tag_no_cache(
    node: &Node,
) -> Option<Rc<Node /*JSDocReadonlyTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_readonly_tag, Some(true))
}

pub fn get_jsdoc_override_tag_no_cache(node: &Node) -> Option<Rc<Node /*JSDocOverrideTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_override_tag, Some(true))
}

pub fn get_jsdoc_deprecated_tag(node: &Node) -> Option<Rc<Node /*JSDocDeprecatedTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_deprecated_tag, None)
}

pub(crate) fn get_jsdoc_deprecated_tag_no_cache(
    node: &Node,
) -> Option<Rc<Node /*JSDocDeprecatedTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_deprecated_tag, Some(true))
}

pub fn get_jsdoc_enum_tag(node: &Node) -> Option<Rc<Node /*JSDocEnumTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_enum_tag, None)
}

pub fn get_jsdoc_this_tag(node: &Node) -> Option<Rc<Node /*JSDocThisTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_this_tag, None)
}

pub fn get_jsdoc_return_tag(node: &Node) -> Option<Rc<Node /*JSDocReturnTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_return_tag, None)
}

pub fn get_jsdoc_template_tag(node: &Node) -> Option<Rc<Node /*JSDocTemplateTag*/>> {
    get_first_jsdoc_tag(node, is_jsdoc_template_tag, None)
}

pub fn get_jsdoc_type_tag(node: &Node) -> Option<Rc<Node /*JSDocTypeTag*/>> {
    let tag = get_first_jsdoc_tag(node, is_jsdoc_type_tag, None);
    // if matches!(tag, Some(tag) if matches!(tag.as_jsdoc_type_tag().type_expression, Some(type_expression) /*if type_expression.type_.is_some()*/))
    if tag.is_some() {
        return tag;
    }
    None
}

pub fn get_jsdoc_type(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let mut tag = get_first_jsdoc_tag(node, is_jsdoc_type_tag, None);
    if tag.is_none() && is_parameter(node) {
        tag = find(&get_jsdoc_parameter_tags(node), |tag| {
            tag.as_jsdoc_property_like_tag().type_expression.is_some()
        });
    }

    tag.and_then(|tag| tag.as_jsdoc_property_like_tag().type_expression)
        .map(|type_expression| type_expression.as_jsdoc_type_expression().type_.clone())
}

pub fn get_jsdoc_return_type(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let return_tag = get_jsdoc_return_tag(node);
    if let Some(return_tag) = return_tag {
        let return_tag_as_jsdoc_return_tag = return_tag.as_jsdoc_return_tag();
        if let Some(return_tag_as_jsdoc_return_tag_type_expression) =
            return_tag_as_jsdoc_return_tag.type_expression.as_ref()
        {
            return Some(return_tag_as_jsdoc_return_tag_type_expression.type_.clone());
        }
    }
    let type_tag = get_jsdoc_type_tag(node);
    if let Some(type_tag) = type_tag {
        // && typeTag.typeExpression
        let type_ = &type_tag
            .as_jsdoc_type_tag()
            .type_expression
            .as_jsdoc_type_expression()
            .type_;
        if is_type_literal_node(&**type_) {
            let sig = find(
                &type_.as_type_literal_node().members,
                is_call_signature_declaration,
            );
            return sig.map(|sig| sig.as_signature_declaration().maybe_type());
        }
        if is_function_type_node(&**type_) || is_jsdoc_function_type(&**type_) {
            return type_.as_has_type().maybe_type();
        }
    }
    None
}

fn get_jsdoc_tags_worker(node: &Node, no_cache: Option<bool>) -> Vec<Rc<Node /*JSDocTag*/>> {
    let mut tags: Option<Vec<Rc<Node>>> = node.maybe_js_doc_cache().clone();
    if tags.is_none() || no_cache.unwrap_or(false) {
        let comments = get_jsdoc_comments_and_tags(node, no_cache);
        Debug_.assert(
            comments.len() < 2 || !Rc::ptr_eq(&comments[0], &comments[1]),
            None,
        );
        tags = Some(flat_map(Some(comments), |j, _| {
            if is_jsdoc(&*j) {
                j.as_jsdoc()
                    .tags
                    .as_ref()
                    .map_or(vec![], |tags| tags.iter().map(Clone::clone).collect())
            } else {
                vec![j]
            }
        }));
        if !no_cache.unwrap_or(false) {
            node.set_js_doc_cache(tags.clone().unwrap());
        }
    }
    tags.unwrap()
}

pub fn get_jsdoc_tags(node: &Node) -> Vec<Rc<Node /*JSDocTag*/>> {
    get_jsdoc_tags_worker(node, Some(false))
}

pub(crate) fn get_jsdoc_tags_no_cache(node: &Node) -> Vec<Rc<Node /*JSDocTag*/>> {
    get_jsdoc_tags_worker(node, Some(true))
}

fn get_first_jsdoc_tag<TPredicate: FnMut(&Node /*JSDocTag*/) -> bool>(
    node: &Node,
    mut predicate: TPredicate,
    no_cache: Option<bool>,
) -> Option<Rc<Node>> {
    find(&get_jsdoc_tags_worker(node, no_cache), |element, _| {
        predicate(element)
    })
    .map(Clone::clone)
}

pub fn get_all_jsdoc_tags<TPredicate: FnMut(&Node /*JSDocTag*/) -> bool>(
    node: &Node,
    predicate: TPredicate,
) -> Vec<Rc<Node>> {
    get_jsdoc_tags(node)
        .into_iter()
        .filter(|rc_ref| predicate(rc_ref))
        .collect()
}

pub fn get_all_jsdoc_tags_of_kind(node: &Node, kind: SyntaxKind) -> Vec<Rc<Node /*JSDocTag*/>> {
    get_jsdoc_tags(node)
        .into_iter()
        .filter(|rc_ref| rc_ref.kind() == kind)
        .collect()
}

pub fn get_effective_type_parameter_declarations(
    node: &Node,
) -> Vec<Rc<Node /*TypeParameterDeclaration*/>> {
    if let Some(type_parameters) = node.as_has_type_parameters().maybe_type_parameters() {
        return type_parameters.into();
    }
    vec![]
}

pub fn is_member_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier || node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn skip_partially_emitted_expressions(node: &Node) -> Rc<Node> {
    skip_outer_expressions(
        node,
        Some(OuterExpressionKinds::PartiallyEmittedExpressions),
    )
}

pub fn is_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstLiteralToken <= kind && kind <= SyntaxKind::LastLiteralToken
}

pub fn is_template_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstTemplateToken <= kind && kind <= SyntaxKind::LastTemplateToken
}

pub fn is_modifier_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AbstractKeyword
            | SyntaxKind::AsyncKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::DefaultKeyword
            | SyntaxKind::ExportKeyword
            | SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::OverrideKeyword
    )
}

pub fn is_property_name<TNode: NodeInterface>(node: &TNode) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::ComputedPropertyName
    )
}

pub fn is_function_like<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    node.map_or(false, |node| is_function_like_kind(node.borrow().kind()))
}

pub fn is_function_like_or_class_static_block_declaration<TNodeRef: Borrow<Node>>(
    node: Option<TNodeRef>,
) -> bool {
    match node {
        Some(node) => {
            let node = node.borrow();
            is_function_like_kind(node.kind()) || is_class_static_block_declaration(node)
        }
        None => false,
    }
}

fn is_function_like_declaration_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
    )
}

fn is_function_like_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::JSDocSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::FunctionType
        | SyntaxKind::JSDocFunctionType
        | SyntaxKind::ConstructorType => true,
        _ => is_function_like_declaration_kind(kind),
    }
}

pub fn is_function_or_module_block<TNode: NodeInterface>(node: &TNode) -> bool {
    is_source_file(node)
        || is_module_block(node)
        || is_block(node) && is_function_like(node.maybe_parent())
}

pub fn is_class_like(node: &Node) -> bool {
    /*node &&*/
    matches!(
        node.kind(),
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression
    )
}

pub fn is_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    if true {
        let kind = node.kind();
        return kind == SyntaxKind::ArrayBindingPattern || kind == SyntaxKind::ObjectBindingPattern;
    }

    false
}

pub(crate) fn is_left_hand_side_expression(node: &Node) -> bool {
    is_left_hand_side_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_left_hand_side_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::Identifier
        | SyntaxKind::NumericLiteral
        | SyntaxKind::FalseKeyword
        | SyntaxKind::TrueKeyword => true,
        _ => false,
    }
}

fn is_unary_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PrefixUnaryExpression => true,
        _ => is_left_hand_side_expression_kind(kind),
    }
}

pub fn is_expression(node: &Node) -> bool {
    is_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        _ => is_unary_expression_kind(kind),
    }
}

pub fn has_jsdoc_nodes<TNode: NodeInterface>(node: &TNode) -> bool {
    node.maybe_js_doc().map_or(false, |jsdoc| !jsdoc.is_empty())
}

pub fn has_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    node.node_wrapper()
        .maybe_as_has_initializer()
        .and_then(|node| node.maybe_initializer())
        .is_some()
}

pub fn has_only_expression_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    match node.kind() {
        SyntaxKind::VariableDeclaration
        | SyntaxKind::Parameter
        | SyntaxKind::BindingElement
        | SyntaxKind::PropertySignature
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::EnumMember => true,
        _ => false,
    }
}

pub fn is_string_literal_like<TNode: NodeInterface>(node: &TNode) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral
    )
}
