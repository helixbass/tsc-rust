use gc::{Finalize, Gc, Trace};

use crate::{
    compiler::scanner::skip_trivia, filter, get_factory, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_parse_tree_node, get_trailing_comment_ranges,
    is_source_file_not_json, last, maybe_concatenate, maybe_for_each_bool, string_contains,
    transform_nodes, CommentRange, Diagnostic, EmitHost, EmitResolver, Node, NodeBuilderFlags,
    NodeInterface, ReadonlyTextRange, ScriptReferenceHost, SourceFileLike, SyntaxKind, TextRange,
    TransformationContext, TransformationResult, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface,
};

pub fn get_declaration_diagnostics(
    host: Gc<Box<dyn EmitHost>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    file: Option<&Node /*SourceFile*/>,
) -> Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>> {
    let compiler_options = ScriptReferenceHost::get_compiler_options(&**host);
    let result = transform_nodes(
        Some(resolver),
        Some(host.clone()),
        get_factory(),
        compiler_options,
        &if let Some(file) = file {
            vec![file.node_wrapper()]
        } else {
            filter(&host.get_source_files(), |source_file: &Gc<Node>| {
                is_source_file_not_json(source_file)
            })
        },
        &[transform_declarations()],
        false,
    );
    result.diagnostics()
}

fn has_internal_annotation(
    range: &CommentRange,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    let current_source_file_text = current_source_file.as_source_file().text();
    let comment =
        &current_source_file_text[range.pos().try_into().unwrap()..range.end().try_into().unwrap()];
    string_contains(comment, "@internal")
}

pub fn is_internal_declaration(
    node: &Node,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    let parse_tree_node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
    if let Some(parse_tree_node) = parse_tree_node
        .as_ref()
        .filter(|parse_tree_node| parse_tree_node.kind() == SyntaxKind::Parameter)
    {
        let param_idx = parse_tree_node
            .parent()
            .as_signature_declaration()
            .parameters()
            .into_iter()
            .position(|parameter: &Gc<Node>| Gc::ptr_eq(parameter, parse_tree_node));
        let previous_sibling = if let Some(param_idx) = param_idx.filter(|param_idx| *param_idx > 0)
        {
            parse_tree_node
                .parent()
                .as_signature_declaration()
                .parameters()
                .get(param_idx - 1)
                .cloned()
        } else {
            None
        };
        let text = current_source_file.as_source_file().text_as_chars();
        let comment_ranges = if let Some(previous_sibling) = previous_sibling.as_ref() {
            maybe_concatenate(
                get_trailing_comment_ranges(
                    &text,
                    skip_trivia(
                        &text,
                        previous_sibling.end() + 1,
                        Some(false),
                        Some(true),
                        None,
                    ),
                ),
                get_leading_comment_ranges(&text, node.pos()),
            )
        } else {
            get_trailing_comment_ranges(
                &text,
                skip_trivia(&text, node.pos(), Some(false), Some(true), None),
            )
        };
        return matches!(
            comment_ranges,
            Some(comment_ranges) if !comment_ranges.is_empty() &&
                has_internal_annotation(
                    last(&comment_ranges),
                    current_source_file
                )
        );
    }
    let leading_comment_ranges = parse_tree_node.and_then(|ref parse_tree_node| {
        get_leading_comment_ranges_of_node(parse_tree_node, current_source_file)
    });
    maybe_for_each_bool(
        leading_comment_ranges.as_ref(),
        |range: &CommentRange, _| has_internal_annotation(range, current_source_file),
    )
}

fn declaration_emit_node_builder_flags() -> NodeBuilderFlags {
    NodeBuilderFlags::MultilineObjectLiterals
        | NodeBuilderFlags::WriteClassExpressionAsTypeLiteral
        | NodeBuilderFlags::UseTypeOfFunction
        | NodeBuilderFlags::UseStructuralFallback
        | NodeBuilderFlags::AllowEmptyTuple
        | NodeBuilderFlags::GenerateNamesForShadowedTypeParams
        | NodeBuilderFlags::NoTruncation
}

#[derive(Trace, Finalize)]
struct TransformDeclarations {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformDeclarations {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformDeclarations {
    fn call(&self, node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformDeclarationsFactory {}

impl TransformDeclarationsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformDeclarationsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformDeclarations::new(context)))
    }
}

pub fn transform_declarations() -> TransformerFactory {
    Gc::new(Box::new(TransformDeclarationsFactory::new()))
}
