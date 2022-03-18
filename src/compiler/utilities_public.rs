use regex::Regex;
use serde_json;
use std::borrow::{Borrow, Cow};
use std::cmp;
use std::collections::HashMap;
use std::ops::BitOrAssign;
use std::ptr;
use std::rc::Rc;

use crate::{
    combine_paths, contains, create_compiler_diagnostic, entity_name_to_string, every, find,
    flat_map, get_assignment_declaration_kind, get_directory_path, get_effective_modifier_flags,
    get_effective_modifier_flags_always_include_jsdoc,
    get_element_or_property_access_argument_expression_or_name, get_emit_script_target,
    get_jsdoc_comments_and_tags, get_jsdoc_type_parameter_declarations, has_syntactic_modifier,
    is_access_expression, is_ambient_module, is_any_import_or_re_export, is_arrow_function,
    is_bindable_static_element_access_expression, is_binding_element, is_call_expression,
    is_call_signature_declaration, is_class_expression, is_class_static_block_declaration,
    is_element_access_expression, is_export_assignment, is_export_declaration, is_export_specifier,
    is_function_block, is_function_expression, is_function_type_node, is_identifier,
    is_import_specifier, is_in_js_file, is_jsdoc, is_jsdoc_augments_tag, is_jsdoc_class_tag,
    is_jsdoc_deprecated_tag, is_jsdoc_enum_tag, is_jsdoc_function_type, is_jsdoc_implements_tag,
    is_jsdoc_override_tag, is_jsdoc_parameter_tag, is_jsdoc_private_tag, is_jsdoc_protected_tag,
    is_jsdoc_public_tag, is_jsdoc_readonly_tag, is_jsdoc_return_tag, is_jsdoc_signature,
    is_jsdoc_template_tag, is_jsdoc_this_tag, is_jsdoc_type_alias, is_jsdoc_type_literal,
    is_jsdoc_type_tag, is_non_null_expression, is_not_emitted_statement, is_omitted_expression,
    is_parameter, is_partially_emitted_expression, is_private_identifier,
    is_property_access_expression, is_property_declaration, is_rooted_disk_path, is_string_literal,
    is_type_literal_node, is_type_node_kind, is_type_reference_node, is_variable_declaration_list,
    is_variable_statement, is_white_space_like, modifier_to_flag, normalize_path, path_is_relative,
    set_localized_diagnostic_messages, set_ui_locale, skip_outer_expressions, some,
    AssignmentDeclarationKind, CharacterCodes, CompilerOptions, Debug_, Diagnostics,
    GeneratedIdentifierFlags, HasTypeParametersInterface, ModifierFlags, NamedDeclarationInterface,
    Node, NodeArray, NodeFlags, NodeInterface, OuterExpressionKinds, Push, ScriptTarget, Symbol,
    SymbolInterface, SyntaxKind, System, TextChangeRange, TextRange, TextSpan, __String,
    compare_diagnostics, is_block, is_module_block, is_source_file, sort_and_deduplicate,
    Diagnostic, SortedArray,
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

pub fn create_text_span(start: isize, length: isize) -> TextSpan {
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
    if is_binding_pattern(Some(node)) {
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

pub fn get_combined_modifier_flags(node: &Node /*Declaration*/) -> ModifierFlags {
    get_combined_flags(node, get_effective_modifier_flags)
}

pub(crate) fn get_combined_node_flags_always_include_jsdoc(
    node: &Node, /*Declaration*/
) -> ModifierFlags {
    get_combined_flags(node, get_effective_modifier_flags_always_include_jsdoc)
}

pub fn get_combined_node_flags(node: &Node) -> NodeFlags {
    get_combined_flags(node, |n| n.flags())
}

lazy_static! {
    pub static ref supported_locale_directories: Vec<&'static str> = vec![
        "cs", "de", "es", "fr", "it", "ja", "ko", "pl", "pt-br", "ru", "tr", "zh-cn", "zh-tw",
    ];
}

pub fn validate_locale_and_set_language(
    locale: &str,
    sys: &dyn System,
    mut errors: Option<&mut Push<Rc<Diagnostic>>>,
) {
    let lower_case_locale = locale.to_lowercase();
    lazy_static! {
        static ref regex: Regex = Regex::new(r"^([a-z]+)([_\-]([a-z]+))?$").unwrap();
    }
    let match_result = regex.captures(&lower_case_locale);

    if match_result.is_none() {
        if let Some(errors) = errors {
            errors.push(Rc::new(create_compiler_diagnostic(&Diagnostics::Locale_must_be_of_the_form_language_or_language_territory_For_example_0_or_1, Some(vec!["en".to_owned(), "ja-jp".to_owned()])).into()));
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

fn try_set_language_and_territory(
    sys: &dyn System,
    language: &str,
    territory: Option<&str>,
    errors: &mut Option<&mut Push<Rc<Diagnostic>>>,
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

    let file_contents = match sys.read_file(&file_path) {
        Err(_) => {
            if let Some(errors) = errors {
                errors.push(Rc::new(
                    create_compiler_diagnostic(
                        &Diagnostics::Unable_to_open_file_0,
                        Some(vec![file_path]),
                    )
                    .into(),
                ));
            }
            return false;
        }
        Ok(file_contents) => file_contents,
    };
    let parsed_file_contents: serde_json::Result<HashMap<String, String>> =
        serde_json::from_str(&file_contents);
    if parsed_file_contents.is_err() {
        if let Some(errors) = errors {
            errors.push(Rc::new(
                create_compiler_diagnostic(
                    &Diagnostics::Corrupted_locale_file_0,
                    Some(vec![file_path]),
                )
                .into(),
            ));
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
    TNode: Borrow<Node>,
    TCallbackReturn: Into<FindAncestorCallbackReturn>,
    TCallback: FnMut(&Node) -> TCallbackReturn,
>(
    node: Option<TNode>,
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

pub fn get_parse_tree_node<TNode: Borrow<Node>, TNodeTest: FnOnce(&Node) -> bool>(
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
                Some(node_test) => node_test(&node_present),
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
    match symbol.maybe_value_declaration().as_ref() {
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
        Node::VariableStatement(host_node) => {
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
        Node::ExpressionStatement(host_node) => {
            let mut expr = host_node.expression.clone();
            match &*expr {
                Node::BinaryExpression(expr_as_binary_expression)
                    if expr_as_binary_expression.operator_token.kind()
                        == SyntaxKind::EqualsToken =>
                {
                    expr = expr_as_binary_expression.left.clone();
                }
                _ => (),
            }
            match &*expr {
                Node::PropertyAccessExpression(expr) => {
                    return Some(expr.name());
                }
                Node::ElementAccessExpression(expr) => {
                    let arg = expr.argument_expression.clone();
                    if is_identifier(&*arg) {
                        return Some(arg);
                    }
                }
                _ => (),
            }
        }
        Node::ParenthesizedExpression(host_node) => {
            return get_declaration_identifier(&host_node.expression);
        }
        Node::LabeledStatement(host_node) => {
            if is_declaration(&host_node.statement) || is_expression(&host_node.statement) {
                return get_declaration_identifier(&host_node.statement);
            }
        }
        _ => (),
    }
    None
}

fn get_declaration_identifier(
    node: &Node, /*Declaration | Expression*/
) -> Option<Rc<Node /*Identifier*/>> {
    let name = get_name_of_declaration(Some(node));
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

pub(crate) fn get_non_assigned_name_of_declaration(
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
                    return Some(expr.as_call_expression().arguments[1].clone());
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
        _ => (),
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
        Node::BinaryExpression(node_parent) => {
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
    // if matches!(tag, Some(tag) if matches!(tag.as_jsdoc_type_like_tag().type_expression, Some(type_expression) /*if type_expression.type_.is_some()*/))
    if tag.is_some() {
        return tag;
    }
    None
}

pub fn get_jsdoc_type(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let mut tag = get_first_jsdoc_tag(node, is_jsdoc_type_tag, None);
    if tag.is_none() && is_parameter(node) {
        tag = find(&get_jsdoc_parameter_tags(node), |tag, _| {
            tag.as_jsdoc_property_like_tag().type_expression.is_some()
        })
        .map(Clone::clone);
    }

    tag.and_then(|tag| tag.as_jsdoc_property_like_tag().type_expression.clone())
        .map(|type_expression| type_expression.as_jsdoc_type_expression().type_.clone())
}

pub fn get_jsdoc_return_type(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let return_tag = get_jsdoc_return_tag(node);
    if let Some(return_tag) = return_tag {
        let return_tag_as_jsdoc_return_tag = return_tag.as_jsdoc_type_like_tag();
        if let Some(return_tag_as_jsdoc_return_tag_type_expression) = return_tag_as_jsdoc_return_tag
            .maybe_type_expression()
            .as_ref()
        {
            return Some(
                return_tag_as_jsdoc_return_tag_type_expression
                    .as_jsdoc_type_expression()
                    .type_
                    .clone(),
            );
        }
    }
    let type_tag = get_jsdoc_type_tag(node);
    if let Some(type_tag) = type_tag {
        // && typeTag.typeExpression
        let type_tag_type_expression = &type_tag.as_jsdoc_type_like_tag().type_expression();
        let type_ = &type_tag_type_expression.as_jsdoc_type_expression().type_;
        if is_type_literal_node(&**type_) {
            let sig = find(&type_.as_type_literal_node().members, |node, _| {
                is_call_signature_declaration(&**node)
            });
            return sig.and_then(|sig| sig.as_signature_declaration().maybe_type());
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
            node.set_js_doc_cache(tags.clone());
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
    mut predicate: TPredicate,
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

#[derive(Debug)]
pub enum StrOrNodeArrayRef<'a> {
    Str(&'a str),
    NodeArray(&'a NodeArray),
}

impl<'a> From<&'a str> for StrOrNodeArrayRef<'a> {
    fn from(value: &'a str) -> Self {
        Self::Str(value)
    }
}

impl<'a> From<&'a NodeArray> for StrOrNodeArrayRef<'a> {
    fn from(value: &'a NodeArray) -> Self {
        Self::NodeArray(value)
    }
}

pub fn get_text_of_jsdoc_comment<'a, TComment: Into<StrOrNodeArrayRef<'a>>>(
    comment: Option<TComment>,
) -> Option<Cow<'a, str>> {
    match comment {
        Some(comment) => match comment.into() {
            StrOrNodeArrayRef::Str(comment) => Some(comment.into()),
            StrOrNodeArrayRef::NodeArray(comment) => Some(
                comment
                    .iter()
                    .map(|c| {
                        if c.kind() == SyntaxKind::JSDocText {
                            c.as_jsdoc_text().text.clone()
                        } else {
                            let c_as_jsdoc_link_like = c.as_jsdoc_link_like();
                            format!(
                                "{{@link {}{}}}",
                                if let Some(c_name) = c_as_jsdoc_link_like.maybe_name() {
                                    format!("{} ", entity_name_to_string(&c_name))
                                } else {
                                    "".to_owned()
                                },
                                c_as_jsdoc_link_like.text()
                            )
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("")
                    .into(),
            ),
        },
        _ => None,
    }
}

pub fn get_effective_type_parameter_declarations(
    node: &Node,
) -> Vec<Rc<Node /*TypeParameterDeclaration*/>> {
    if is_jsdoc_signature(node) {
        return vec![];
    }
    if is_jsdoc_type_alias(node) {
        Debug_.assert(node.parent().kind() == SyntaxKind::JSDocComment, None);
        return flat_map(node.parent().as_jsdoc().tags.as_ref(), |tag, _| {
            if is_jsdoc_template_tag(&**tag) {
                tag.as_jsdoc_template_tag().type_parameters.to_vec()
            } else {
                /*None*/
                vec![]
            }
        });
    }
    if let Some(type_parameters) = node
        .as_has_type_parameters()
        .maybe_type_parameters()
        .as_ref()
    {
        return type_parameters.into();
    }
    if is_in_js_file(Some(node)) {
        let decls = get_jsdoc_type_parameter_declarations(node);
        if !decls.is_empty() {
            return decls;
        }
        let type_tag = get_jsdoc_type(node);
        if let Some(type_tag) = type_tag {
            if is_function_type_node(&*type_tag) {
                let type_tag_as_function_type_node = type_tag.as_function_type_node();
                if let Some(type_tag_type_parameters) = type_tag_as_function_type_node
                    .maybe_type_parameters()
                    .as_ref()
                {
                    return type_tag_type_parameters.to_vec();
                }
            }
        }
    }
    vec![]
}

pub fn get_effective_constraint_of_type_parameter(
    node: &Node, /*TypeParameterDeclaration*/
) -> Option<Rc<Node /*TypeNode*/>> {
    if let Some(node_constraint) = node.as_type_parameter_declaration().constraint.as_ref() {
        return Some(node_constraint.clone());
    }
    if is_jsdoc_template_tag(&*node.parent()) {
        let node_parent = node.parent();
        let node_parent_as_jsdoc_template_tag = node_parent.as_jsdoc_template_tag();
        if !node_parent_as_jsdoc_template_tag.type_parameters.is_empty()
            && ptr::eq(node, &*node_parent_as_jsdoc_template_tag.type_parameters[0])
        {
            return node_parent_as_jsdoc_template_tag.constraint.clone();
        }
    }
    None
}

pub fn is_member_name(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier
    )
}

pub(crate) fn is_get_or_set_accessor_declaration(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::SetAccessor | SyntaxKind::GetAccessor
    )
}

pub fn is_property_access_chain(node: &Node) -> bool {
    is_property_access_expression(node) && node.flags().intersects(NodeFlags::OptionalChain)
}

pub fn is_element_access_chain(node: &Node) -> bool {
    is_element_access_expression(node) && node.flags().intersects(NodeFlags::OptionalChain)
}

pub fn is_call_chain(node: &Node) -> bool {
    is_call_expression(node) && node.flags().intersects(NodeFlags::OptionalChain)
}

pub fn is_optional_chain(node: &Node) -> bool {
    node.flags().intersects(NodeFlags::OptionalChain)
        && matches!(
            node.kind(),
            SyntaxKind::PropertyAccessExpression
                | SyntaxKind::ElementAccessExpression
                | SyntaxKind::CallExpression
                | SyntaxKind::NonNullExpression
        )
}

pub(crate) fn is_optional_chain_root(node: &Node /*OptionalChain*/) -> bool {
    is_optional_chain(node)
        && !is_non_null_expression(node)
        && node
            .as_has_question_dot_token()
            .maybe_question_dot_token()
            .is_some()
}

pub(crate) fn is_expression_of_optional_chain_root(node: &Node /*OptionalChain*/) -> bool {
    is_optional_chain_root(&node.parent())
        && ptr::eq(&*node.parent().as_has_expression().expression(), node)
}

pub(crate) fn is_outermost_optional_chain(node: &Node /*OptionalChain*/) -> bool {
    !is_optional_chain(&node.parent())
        || is_optional_chain_root(&node.parent())
        || !ptr::eq(node, &*node.parent().as_has_expression().expression())
}

pub fn is_nullish_coalesce(node: &Node) -> bool {
    match node {
        Node::BinaryExpression(node) => {
            node.operator_token.kind() == SyntaxKind::QuestionQuestionToken
        }
        _ => false,
    }
}

pub fn is_const_type_reference(node: &Node) -> bool {
    // match node {
    //     Node::TypeNode(TypeNode::TypeReferenceNode(node)) => {
    //         match &*node.type_name {
    //             Node::Identifier(type_name) =>
    //                 type_name.escaped_text.eq_str("const") && node.type_arguments.is_none(),
    //             _ => false,
    //         }
    //     }
    //     _ => false,
    // }
    if !is_type_reference_node(node) {
        return false;
    }
    let node_as_type_reference_node = node.as_type_reference_node();
    is_identifier(&*node_as_type_reference_node.type_name)
        && node_as_type_reference_node
            .type_name
            .as_identifier()
            .escaped_text
            .eq_str("const")
        && node_as_type_reference_node.type_arguments.is_none()
}

pub fn skip_partially_emitted_expressions(node: &Node) -> Rc<Node> {
    skip_outer_expressions(
        node,
        Some(OuterExpressionKinds::PartiallyEmittedExpressions),
    )
}

pub fn is_non_null_chain(node: &Node) -> bool {
    is_non_null_expression(node) && node.flags().intersects(NodeFlags::OptionalChain)
}

pub fn is_break_or_continue_statement(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::BreakStatement | SyntaxKind::ContinueStatement
    )
}

pub fn is_named_export_bindings(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::NamespaceExport | SyntaxKind::NamedExports
    )
}

pub fn is_unparsed_text_like(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::UnparsedText | SyntaxKind::UnparsedInternalText
    )
}

pub fn is_unparsed_node(node: &Node) -> bool {
    is_unparsed_text_like(node)
        || matches!(
            node.kind(),
            SyntaxKind::UnparsedPrologue | SyntaxKind::UnparsedSyntheticReference
        )
}

pub fn is_jsdoc_property_like_tag(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JSDocPropertyTag | SyntaxKind::JSDocParameterTag
    )
}

pub(crate) fn is_node(node: &Node) -> bool {
    is_node_kind(node.kind())
}

pub(crate) fn is_node_kind(kind: SyntaxKind) -> bool {
    kind >= SyntaxKind::FirstNode
}

pub fn is_token_kind(kind: SyntaxKind) -> bool {
    kind >= SyntaxKind::FirstToken && kind <= SyntaxKind::LastToken
}

pub fn is_token(n: &Node) -> bool {
    is_token_kind(n.kind())
}

// TODO: is isNodeArray() needed?

pub fn is_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstLiteralToken <= kind && kind <= SyntaxKind::LastLiteralToken
}

pub fn is_literal_expression(node: &Node) -> bool {
    is_literal_kind(node.kind())
}

pub(crate) fn is_template_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstTemplateToken <= kind && kind <= SyntaxKind::LastTemplateToken
}

pub fn is_template_literal_token(node: &Node) -> bool {
    is_template_literal_kind(node.kind())
}

pub fn is_template_middle_or_template_tail(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::TemplateMiddle | SyntaxKind::TemplateTail
    )
}

pub fn is_import_or_export_specifier(node: &Node) -> bool {
    is_import_specifier(node) || is_export_specifier(node)
}

pub fn is_type_only_import_or_export_declaration(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::ImportSpecifier | SyntaxKind::ExportSpecifier => {
            node.as_has_is_type_only().is_type_only()
                || node.parent().parent().as_has_is_type_only().is_type_only()
        }
        SyntaxKind::NamespaceImport => node.parent().as_has_is_type_only().is_type_only(),
        SyntaxKind::ImportClause | SyntaxKind::ImportEqualsDeclaration => {
            node.as_has_is_type_only().is_type_only()
        }
        _ => false,
    }
}

pub fn is_assertion_key(node: &Node) -> bool {
    is_string_literal(node) || is_identifier(node)
}

pub fn is_string_text_containing_node(node: &Node) -> bool {
    node.kind() == SyntaxKind::StringLiteral || is_template_literal_kind(node.kind())
}

pub(crate) fn is_generated_identifier(node: &Node) -> bool {
    is_identifier(node)
        && matches!(node.as_identifier().maybe_auto_generate_flags(), Some(auto_generate_flags) if auto_generate_flags.intersects(GeneratedIdentifierFlags::KindMask))
}

pub(crate) fn is_private_identifier_class_element_declaration(node: &Node) -> bool {
    (is_property_declaration(node) || is_method_or_accessor(node))
        && is_private_identifier(&*node.as_named_declaration().name())
}

pub(crate) fn is_private_identifier_property_access_expression(node: &Node) -> bool {
    is_property_access_expression(node)
        && is_private_identifier(&*node.as_property_access_expression().name())
}

pub(crate) fn is_modifier_kind(kind: SyntaxKind) -> bool {
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

pub(crate) fn is_parameter_property_modifier(kind: SyntaxKind) -> bool {
    modifier_to_flag(kind).intersects(ModifierFlags::ParameterPropertyModifier)
}

pub(crate) fn is_class_member_modifier(id_token: SyntaxKind) -> bool {
    is_parameter_property_modifier(id_token)
        || matches!(
            id_token,
            SyntaxKind::StaticKeyword | SyntaxKind::OverrideKeyword
        )
}

pub fn is_modifier(node: &Node) -> bool {
    is_modifier_kind(node.kind())
}

pub fn is_entity_name(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::QualifiedName | SyntaxKind::Identifier
    )
}

pub fn is_property_name(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::ComputedPropertyName
    )
}

pub fn is_binding_name(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier | SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern
    )
}

pub fn is_function_like<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    node.map_or(false, |node| is_function_like_kind(node.borrow().kind()))
}

pub(crate) fn is_function_like_or_class_static_block_declaration<TNodeRef: Borrow<Node>>(
    node: Option<TNodeRef>,
) -> bool {
    node.map_or(false, |node| {
        let node = node.borrow();
        is_function_like_kind(node.kind()) || is_class_static_block_declaration(node)
    })
}

pub(crate) fn is_function_like_declaration(node: &Node) -> bool {
    /*node &&*/
    is_function_like_declaration_kind(node.kind())
}

pub(crate) fn is_boolean_literal(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword
    )
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

pub(crate) fn is_function_like_kind(kind: SyntaxKind) -> bool {
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

pub(crate) fn is_function_or_module_block(node: &Node) -> bool {
    is_source_file(node)
        || is_module_block(node)
        || is_block(node) && is_function_like(node.maybe_parent())
}

pub fn is_class_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Constructor
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::IndexSignature
            | SyntaxKind::ClassStaticBlockDeclaration
            | SyntaxKind::SemicolonClassElement
    )
}

pub fn is_class_like(node: &Node) -> bool {
    /*node &&*/
    matches!(
        node.kind(),
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression
    )
}

pub fn is_accessor(node: &Node) -> bool {
    /*node && */
    matches!(
        node.kind(),
        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
    )
}

pub(crate) fn is_method_or_accessor(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
    )
}

pub fn is_type_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ConstructSignature
            | SyntaxKind::CallSignature
            | SyntaxKind::PropertySignature
            | SyntaxKind::MethodSignature
            | SyntaxKind::IndexSignature
    )
}

pub fn is_class_or_type_element(node: &Node) -> bool {
    is_type_element(node) || is_class_element(node)
}

pub fn is_object_literal_element_like(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAssignment
            | SyntaxKind::ShorthandPropertyAssignment
            | SyntaxKind::SpreadAssignment
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
    )
}

pub fn is_type_node(node: &Node) -> bool {
    is_type_node_kind(node.kind())
}

pub fn is_function_or_constructor_type_node(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionType | SyntaxKind::ConstructorType
    )
}

pub fn is_binding_pattern<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    if let Some(node) = node {
        let kind = node.borrow().kind();
        return matches!(
            kind,
            SyntaxKind::ArrayBindingPattern | SyntaxKind::ObjectBindingPattern
        );
    }

    false
}

pub(crate) fn is_assignment_pattern(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ArrayLiteralExpression | SyntaxKind::ObjectLiteralExpression
    )
}

pub(crate) fn is_array_binding_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::BindingElement | SyntaxKind::OmittedExpression
    )
}

pub(crate) fn is_declaration_binding_element(
    binding_element: &Node, /*BindingOrAssignmentElement*/
) -> bool {
    matches!(
        binding_element.kind(),
        SyntaxKind::VariableDeclaration | SyntaxKind::Parameter | SyntaxKind::BindingElement
    )
}

pub(crate) fn is_binding_or_assigment_pattern(
    node: &Node, /*BindingOrAssignmentElementTarget*/
) -> bool {
    is_object_binding_or_assigment_pattern(node) || is_array_binding_or_assigment_pattern(node)
}

pub(crate) fn is_object_binding_or_assigment_pattern(
    node: &Node, /*BindingOrAssignmentElementTarget*/
) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ObjectBindingPattern | SyntaxKind::ObjectLiteralExpression
    )
}

pub(crate) fn is_object_binding_or_assignment_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::BindingElement
            | SyntaxKind::PropertyAssignment
            | SyntaxKind::ShorthandPropertyAssignment
            | SyntaxKind::SpreadAssignment
    )
}

pub(crate) fn is_array_binding_or_assigment_pattern(
    node: &Node, /*BindingOrAssignmentElement*/
) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ArrayBindingPattern | SyntaxKind::ArrayLiteralExpression
    )
}

pub(crate) fn is_property_access_or_qualified_name_or_import_type_node(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::QualifiedName | SyntaxKind::ImportType
    )
}

pub fn is_property_access_or_qualified_name(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::QualifiedName
    )
}

pub fn is_call_like_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JsxOpeningElement
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::CallExpression
            | SyntaxKind::NewExpression
            | SyntaxKind::TaggedTemplateExpression
            | SyntaxKind::Decorator
    )
}

pub fn is_call_or_new_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::CallExpression | SyntaxKind::NewExpression
    )
}

pub fn is_template_literal(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::TemplateExpression | SyntaxKind::NoSubstitutionTemplateLiteral
    )
}

pub(crate) fn is_left_hand_side_expression(node: &Node) -> bool {
    is_left_hand_side_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_left_hand_side_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::PropertyAccessExpression
            | SyntaxKind::ElementAccessExpression
            | SyntaxKind::NewExpression
            | SyntaxKind::CallExpression
            | SyntaxKind::JsxElement
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxFragment
            | SyntaxKind::TaggedTemplateExpression
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::ParenthesizedExpression
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::ClassExpression
            | SyntaxKind::FunctionExpression
            | SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::RegularExpressionLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateExpression
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::ThisKeyword
            | SyntaxKind::TrueKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::NonNullExpression
            | SyntaxKind::MetaProperty
            | SyntaxKind::ImportKeyword
    )
}

pub(crate) fn is_unary_expression(node: &Node) -> bool {
    is_unary_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_unary_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::PostfixUnaryExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::VoidExpression
        | SyntaxKind::AwaitExpression
        | SyntaxKind::TypeAssertionExpression => true,
        _ => is_left_hand_side_expression_kind(kind),
    }
}

pub(crate) fn is_unary_expression_with_write(expr: &Node) -> bool {
    match expr.kind() {
        SyntaxKind::PostfixUnaryExpression => true,
        SyntaxKind::PrefixUnaryExpression => matches!(
            expr.as_prefix_unary_expression().operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ),
        _ => false,
    }
}

pub fn is_expression(node: &Node) -> bool {
    is_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::ConditionalExpression
        | SyntaxKind::YieldExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::BinaryExpression
        | SyntaxKind::SpreadElement
        | SyntaxKind::OmittedExpression
        | SyntaxKind::CommaListExpression
        | SyntaxKind::PartiallyEmittedExpression => true,
        _ => is_unary_expression_kind(kind),
    }
}

pub fn is_assertion_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression
    )
}

pub(crate) fn is_not_emitted_or_partially_emitted_node(node: &Node) -> bool {
    is_not_emitted_statement(node) || is_partially_emitted_expression(node)
}

pub fn is_iteration_statement(node: &Node, look_in_labeled_statements: bool) -> bool {
    match node.kind() {
        SyntaxKind::ForStatement
        | SyntaxKind::ForInStatement
        | SyntaxKind::ForOfStatement
        | SyntaxKind::DoStatement
        | SyntaxKind::WhileStatement => true,
        SyntaxKind::LabeledStatement => {
            look_in_labeled_statements
                && is_iteration_statement(
                    &node.as_labeled_statement().statement,
                    look_in_labeled_statements,
                )
        }
        _ => false,
    }
}

pub(crate) fn is_scope_marker(node: &Node) -> bool {
    is_export_assignment(node) || is_export_declaration(node)
}

pub(crate) fn has_scope_marker(statements: &[Rc<Node /*Statement*/>]) -> bool {
    some(
        Some(statements),
        Some(|statement: &Rc<Node>| is_scope_marker(statement)),
    )
}

pub(crate) fn needs_scope_marker(result: &Node /*Statement*/) -> bool {
    !is_any_import_or_re_export(result)
        && !is_export_assignment(result)
        && !has_syntactic_modifier(result, ModifierFlags::Export)
        && !is_ambient_module(result)
}

pub(crate) fn is_external_module_indicator(result: &Node /*Statement*/) -> bool {
    is_any_import_or_re_export(result)
        || is_export_assignment(result)
        || has_syntactic_modifier(result, ModifierFlags::Export)
}

pub(crate) fn is_for_in_or_of_statement(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement
    )
}

pub(crate) fn is_concise_body(node: &Node) -> bool {
    is_block(node) || is_expression(node)
}

pub(crate) fn is_function_body(node: &Node) -> bool {
    is_block(node)
}

pub(crate) fn is_for_initializer(node: &Node) -> bool {
    is_variable_declaration_list(node) || is_expression(node)
}

pub(crate) fn is_module_body(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ModuleBlock | SyntaxKind::ModuleDeclaration | SyntaxKind::Identifier
    )
}

pub(crate) fn is_namespace_body(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ModuleBlock | SyntaxKind::ModuleDeclaration
    )
}

pub(crate) fn is_jsdoc_namespace_body(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier | SyntaxKind::ModuleDeclaration
    )
}

pub(crate) fn is_named_import_bindings(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::NamedImports | SyntaxKind::NamespaceImport
    )
}

pub(crate) fn is_module_or_enum_declaration(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ModuleDeclaration | SyntaxKind::EnumDeclaration
    )
}

fn is_declaration_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::ArrowFunction
            | SyntaxKind::BindingElement
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::ClassExpression
            | SyntaxKind::ClassStaticBlockDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::EnumMember
            | SyntaxKind::ExportSpecifier
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::GetAccessor
            | SyntaxKind::ImportClause
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::JsxAttribute
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::NamespaceExportDeclaration
            | SyntaxKind::NamespaceImport
            | SyntaxKind::NamespaceExport
            | SyntaxKind::Parameter
            | SyntaxKind::PropertyAssignment
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::SetAccessor
            | SyntaxKind::ShorthandPropertyAssignment
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::TypeParameter
            | SyntaxKind::VariableDeclaration
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocPropertyTag
    )
}

fn is_declaration_statement_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::FunctionDeclaration
            | SyntaxKind::MissingDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::NamespaceExportDeclaration
    )
}

fn is_statement_kind_but_not_declaration_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::BreakStatement
            | SyntaxKind::ContinueStatement
            | SyntaxKind::DebuggerStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::ExpressionStatement
            | SyntaxKind::EmptyStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::IfStatement
            | SyntaxKind::LabeledStatement
            | SyntaxKind::ReturnStatement
            | SyntaxKind::SwitchStatement
            | SyntaxKind::ThrowStatement
            | SyntaxKind::TryStatement
            | SyntaxKind::VariableStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::NotEmittedStatement
            | SyntaxKind::EndOfDeclarationMarker
            | SyntaxKind::MergeDeclarationMarker
    )
}

pub(crate) fn is_declaration(node: &Node) -> bool {
    if node.kind() == SyntaxKind::TypeParameter {
        return node.maybe_parent().map_or(false, |parent| {
            parent.kind() != SyntaxKind::JSDocTemplateTag
        }) || is_in_js_file(Some(node));
    }

    is_declaration_kind(node.kind())
}

pub(crate) fn is_declaration_statement(node: &Node) -> bool {
    is_declaration_statement_kind(node.kind())
}

pub(crate) fn is_statement_but_not_declaration(node: &Node) -> bool {
    is_statement_kind_but_not_declaration_kind(node.kind())
}

pub(crate) fn is_statement(node: &Node) -> bool {
    let kind = node.kind();
    is_statement_kind_but_not_declaration_kind(kind)
        || is_declaration_statement_kind(kind)
        || is_block_statement(node)
}

fn is_block_statement(node: &Node) -> bool {
    if node.kind() != SyntaxKind::Block {
        return false;
    }
    if let Some(node_parent) = node.maybe_parent() {
        if matches!(
            node_parent.kind(),
            SyntaxKind::TryStatement | SyntaxKind::CatchClause
        ) {
            return false;
        }
    }
    !is_function_block(node)
}

pub(crate) fn is_statement_or_block(node: &Node) -> bool {
    let kind = node.kind();
    is_statement_kind_but_not_declaration_kind(kind)
        || is_declaration_statement_kind(kind)
        || kind == SyntaxKind::Block
}

pub(crate) fn is_module_reference(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ExternalModuleReference | SyntaxKind::QualifiedName | SyntaxKind::Identifier
    )
}

pub(crate) fn is_jsx_tag_name_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ThisKeyword | SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression
    )
}

pub(crate) fn is_jsx_child(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JsxElement
            | SyntaxKind::JsxExpression
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxText
            | SyntaxKind::JsxFragment
    )
}

pub(crate) fn is_jsx_attribute_like(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JsxAttribute | SyntaxKind::JsxSpreadAttribute
    )
}

pub(crate) fn is_string_literal_or_jsx_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::StringLiteral | SyntaxKind::JsxExpression
    )
}

pub fn is_jsx_opening_like_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JsxOpeningElement | SyntaxKind::JsxSelfClosingElement
    )
}

pub fn is_case_or_default_clause(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::CaseClause | SyntaxKind::DefaultClause
    )
}

pub(crate) fn is_jsdoc_node(node: &Node) -> bool {
    node.kind() >= SyntaxKind::FirstJSDocNode && node.kind() <= SyntaxKind::LastJSDocNode
}

pub fn is_jsdoc_comment_containing_node(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JSDocComment | SyntaxKind::JSDocNamepathType | SyntaxKind::JSDocText
    ) || is_jsdoc_link_like(node)
        || is_jsdoc_tag(node)
        || is_jsdoc_type_literal(node)
        || is_jsdoc_signature(node)
}

pub(crate) fn is_jsdoc_tag(node: &Node) -> bool {
    node.kind() >= SyntaxKind::FirstJSDocTagNode && node.kind() <= SyntaxKind::LastJSDocTagNode
}

pub fn is_set_accessor(node: &Node) -> bool {
    node.kind() == SyntaxKind::SetAccessor
}

pub fn is_get_accessor(node: &Node) -> bool {
    node.kind() == SyntaxKind::GetAccessor
}

pub(crate) fn has_jsdoc_nodes(node: &Node) -> bool {
    node.maybe_js_doc().map_or(false, |jsdoc| !jsdoc.is_empty())
}

pub(crate) fn has_type(node: &Node) -> bool {
    node.maybe_as_has_type()
        .and_then(|node| node.maybe_type())
        .is_some()
}

pub(crate) fn has_initializer(node: &Node) -> bool {
    node.maybe_as_has_initializer()
        .and_then(|node| node.maybe_initializer())
        .is_some()
}

pub fn has_only_expression_initializer(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::VariableDeclaration
            | SyntaxKind::Parameter
            | SyntaxKind::BindingElement
            | SyntaxKind::PropertySignature
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertyAssignment
            | SyntaxKind::EnumMember
    )
}

pub fn is_object_literal_element(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JsxAttribute | SyntaxKind::JsxSpreadAttribute
    ) || is_object_literal_element_like(node)
}

pub(crate) fn is_type_reference_type(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::TypeReference | SyntaxKind::ExpressionWithTypeArguments
    )
}

const MAX_SMI_X86: usize = 0x3fff_ffff;
pub(crate) fn guess_indentation(lines: &[&str]) -> Option<usize> {
    let mut indentation = MAX_SMI_X86;
    for line in lines {
        if line.is_empty() {
            continue;
        }
        let mut i = 0;
        let mut line_chars = line.chars();
        while i < indentation {
            let ch = line_chars.next();
            if !matches!(ch, Some(ch) if is_white_space_like(ch)) {
                break;
            }
            i += 1;
        }
        if i < indentation {
            indentation = i;
        }
        if indentation == 0 {
            return Some(0);
        }
    }
    if indentation == MAX_SMI_X86 {
        None
    } else {
        Some(indentation)
    }
}

pub fn is_string_literal_like(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral
    )
}

pub fn is_jsdoc_link_like(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JSDocLink | SyntaxKind::JSDocLinkCode | SyntaxKind::JSDocLinkPlain
    )
}
