use bitflags::bitflags;
use squalid::EverythingExt;
use typescript_rust::{
    get_jsdoc_enum_tag, get_module_instance_state, id_arena::Id, is_ambient_module,
    is_break_or_continue_statement, is_identifier, is_in_js_file, is_labeled_statement, HasArena,
    InArena, ModuleInstanceState, Node, NodeInterface, SyntaxKind,
};

bitflags! {
    pub struct SemanticMeaning: u32 {
        const None = 0x0;
        const Value = 0x1;
        const Type = 0x2;
        const Namespace = 0x4;
        const All = Self::Value.bits | Self::Type.bits | Self::Namespace.bits;
    }
}

pub fn get_meaning_from_declaration(node: Id<Node>, arena: &impl HasArena) -> SemanticMeaning {
    match node.ref_(arena).kind() {
        SyntaxKind::VariableDeclaration => {
            if is_in_js_file(Some(&node.ref_(arena))) && get_jsdoc_enum_tag(node, arena).is_some() {
                SemanticMeaning::All
            } else {
                SemanticMeaning::Value
            }
        }

        SyntaxKind::Parameter
        | SyntaxKind::BindingElement
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::ShorthandPropertyAssignment
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::MethodSignature
        | SyntaxKind::Constructor
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::CatchClause
        | SyntaxKind::JsxAttribute => SemanticMeaning::Value,

        SyntaxKind::TypeParameter
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration
        | SyntaxKind::TypeLiteral => SemanticMeaning::Type,

        SyntaxKind::JSDocTypedefTag => {
            if node.ref_(arena).as_jsdoc_typedef_tag().name.is_none() {
                SemanticMeaning::Value | SemanticMeaning::Type
            } else {
                SemanticMeaning::Type
            }
        }

        SyntaxKind::EnumMember | SyntaxKind::ClassDeclaration => {
            SemanticMeaning::Value | SemanticMeaning::Type
        }

        SyntaxKind::ModuleDeclaration => {
            if is_ambient_module(node, arena) {
                SemanticMeaning::Namespace | SemanticMeaning::Value
            } else if get_module_instance_state(node, None, arena)
                == ModuleInstanceState::Instantiated
            {
                SemanticMeaning::Namespace | SemanticMeaning::Value
            } else {
                SemanticMeaning::Namespace
            }
        }

        SyntaxKind::EnumDeclaration
        | SyntaxKind::NamedImports
        | SyntaxKind::ImportSpecifier
        | SyntaxKind::ImportEqualsDeclaration
        | SyntaxKind::ImportDeclaration
        | SyntaxKind::ExportAssignment
        | SyntaxKind::ExportDeclaration => SemanticMeaning::All,

        SyntaxKind::SourceFile => SemanticMeaning::Namespace | SemanticMeaning::Value,

        _ => SemanticMeaning::All,
    }
}

pub fn is_jump_statement_target(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_identifier(&node.ref_(arena))
        && node.ref_(arena).parent().thrush(|node_parent| {
            is_break_or_continue_statement(&node_parent.ref_(arena))
                && node_parent.ref_(arena).as_has_label().maybe_label() == Some(node)
        })
}

pub fn is_label_of_labeled_statement(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_identifier(&node.ref_(arena))
        && node.ref_(arena).parent().thrush(|node_parent| {
            is_labeled_statement(&node_parent.ref_(arena))
                && node_parent.ref_(arena).as_labeled_statement().label == node
        })
}

pub fn is_label_name(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_label_of_labeled_statement(node, arena) || is_jump_statement_target(node, arena)
}
