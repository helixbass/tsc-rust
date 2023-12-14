use std::sync::Arc;

use tree_sitter_lint::{rule, violation, NodeExt, Rule};

pub fn rewrite_self_arena_method_to_ref_method_rule() -> Arc<dyn Rule> {
    rule! {
        name => "rewrite-self-arena-method-to-ref-method",
        fixable => true,
        allow_self_conflicting_fixes => true,
        listeners => [
            r#"
              (call_expression
                 function: (field_expression
                   value: (field_expression
                     value: (self)
                     field: (field_identifier) @type_checker (#eq? @type_checker "type_checker")
                   )
                   field: (field_identifier) @method (#eq? @method "type_mapper")
                 )
                 arguments: (arguments
                   (_) @arg
                 )
              ) @call
            "# => |captures, context| {
                context.report(
                    violation! {
                        message => "self.type_checker.type_mapper(arg) -> arg.ref_(self)",
                        node => captures["call"],
                        fix => |fixer| {
                            fixer.replace_text(
                                captures["call"],
                                &format!("{}.ref_(self)", captures["arg"].text(context))
                            );
                        }
                    }
                );
            }
        ],
        languages => [Rust],
    }
}
