use std::sync::Arc;

use tree_sitter_lint::{rule, violation, Rule};

pub fn rewrite_ref_type_to_id_type_rule() -> Arc<dyn Rule> {
    rule! {
        name => "rewrite-ref-type-to-id-type",
        fixable => true,
        listeners => [
            r#"
              (reference_type
                 type: (type_identifier) @type (#eq? @type "Node")
              ) @ref
            "# => {
                capture_name => "ref",
                callback => |node, context| {
                    context.report(
                        violation! {
                            message => "&Node -> Id<Node>",
                            node => node,
                            fix => |fixer| {
                                fixer.replace_text(node, "Id<Node>");
                            }
                        }
                    );
                }
            }
        ],
        languages => [Rust],
    }
}
