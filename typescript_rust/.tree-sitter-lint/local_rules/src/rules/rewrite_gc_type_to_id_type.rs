use std::sync::Arc;

use tree_sitter_lint::{rule, violation, Rule};

pub fn rewrite_gc_type_to_id_type_rule() -> Arc<dyn Rule> {
    rule! {
        name => "rewrite-gc-type-to-id-type",
        fixable => true,
        listeners => [
            r#"
              (generic_type
                 type: (type_identifier) @gc (#eq? @gc "Gc")
                 type_arguments: (type_arguments
                   (type_identifier) @type (#eq? @type "Symbol")
                 )
              )
            "# => {
                capture_name => "gc",
                callback => |node, context| {
                    context.report(
                        violation! {
                            message => "Gc<Symbol> -> Id<Symbol>",
                            node => node,
                            fix => |fixer| {
                                fixer.replace_text(node, "Id");
                            }
                        }
                    );
                }
            }
        ],
        languages => [Rust],
    }
}
