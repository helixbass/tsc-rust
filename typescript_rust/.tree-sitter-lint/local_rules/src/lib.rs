use std::sync::Arc;

use tree_sitter_lint::Rule;

mod rules;

use rules::{
    rewrite_gc_type_to_id_type_rule, rewrite_ref_type_to_id_type_rule,
    rewrite_self_arena_method_to_ref_method_rule,
};

pub fn get_rules() -> Vec<Arc<dyn Rule>> {
    vec![
        rewrite_gc_type_to_id_type_rule(),
        rewrite_ref_type_to_id_type_rule(),
        rewrite_self_arena_method_to_ref_method_rule(),
    ]
}
