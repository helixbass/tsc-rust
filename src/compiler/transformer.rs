use bitflags::bitflags;

use crate::{
    transform_ecmascript_module, transform_module, transform_node_module, transform_system_module,
    CompilerOptions, CustomTransformers, EmitTransformers, ModuleKind, TransformerFactory,
};

fn get_module_transformer(module_kind: ModuleKind) -> TransformerFactory {
    match module_kind {
        ModuleKind::ESNext | ModuleKind::ES2022 | ModuleKind::ES2020 | ModuleKind::ES2015 => {
            transform_ecmascript_module
        }
        ModuleKind::System => transform_system_module,
        ModuleKind::Node12 | ModuleKind::NodeNext => transform_node_module,
        _ => transform_module,
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum TransformationState {
    Uninitialized,
    Initialized,
    Completed,
    Disposed,
}

bitflags! {
    struct SyntaxKindFeatureFlags: u32 {
        const None = 0;
        const Substitution = 1 << 0;
        const EmitNotifications = 1 << 1;
    }
}

lazy_static! {
    pub static ref no_transformers: EmitTransformers = EmitTransformers::new(vec![], vec![]);
}

pub fn get_transformers(
    compiler_options: &CompilerOptions,
    custom_transformers: Option<&CustomTransformers>,
    emit_only_dts_files: Option<bool>,
) -> EmitTransformers {
    EmitTransformers::new(
        get_script_transformers(compiler_options, custom_transformers, emit_only_dts_files),
        get_declaration_transformers(custom_transformers),
    )
}

fn get_script_transformers(
    compiler_options: &CompilerOptions,
    custom_transformers: Option<&CustomTransformers>,
    emit_only_dts_files: Option<bool>,
) -> Vec<TransformerFactory> {
    unimplemented!()
}

fn get_declaration_transformers(
    custom_transformers: Option<&CustomTransformers>,
) -> Vec<TransformerFactory> {
    unimplemented!()
}
