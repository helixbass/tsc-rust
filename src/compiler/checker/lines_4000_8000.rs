#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::NodeBuilder;
use crate::{
    TypeNode, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
    VariableDeclaration, VariableStatement, __String, bind_source_file, chain_diagnostic_messages,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_printer, create_symbol_table,
    create_text_writer, declaration_name_to_string, escape_leading_underscores, every, factory,
    first_defined, first_or_undefined, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, get_synthetic_factory, has_dynamic_name,
    has_initializer, is_binding_element, is_external_or_common_js_module, is_identifier_text,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, unescape_leading_underscores, using_single_line_string_writer, ArrayTypeNode,
    BaseInterfaceType, BaseIntrinsicType, BaseLiteralType, BaseNodeFactorySynthetic,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectLiteralExpression, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, StringLiteralType,
    Symbol, SymbolFlags, SymbolFormatFlags, SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type,
    TypeChecker, TypeCheckerHost, TypeElement, TypeFlags, TypeInterface, TypeReferenceNode,
    UnionReduction, VariableLikeDeclarationInterface,
};

impl TypeChecker {
    pub(crate) fn symbol_is_value(&self, symbol: Rc<Symbol>) -> bool {
        symbol.flags().intersects(SymbolFlags::Value)
    }

    pub(crate) fn create_type(&self, flags: TypeFlags) -> BaseType {
        let result = (self.Type)(flags);
        result
    }

    pub(crate) fn create_intrinsic_type(
        &self,
        kind: TypeFlags,
        intrinsic_name: &str,
    ) -> BaseIntrinsicType {
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_, intrinsic_name.to_string());
        type_
    }

    pub(crate) fn create_object_type(
        &self,
        object_flags: ObjectFlags,
        symbol: Rc<Symbol>,
    ) -> BaseObjectType {
        let mut type_ = self.create_type(TypeFlags::Object);
        type_.set_symbol(symbol);
        let type_ = BaseObjectType::new(type_, object_flags);
        type_
    }

    pub(crate) fn is_reserved_member_name(&self, name: &__String) -> bool {
        let mut chars = name.chars();
        let mut current_char: Option<char> = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char == CharacterCodes::underscore
                || current_char == CharacterCodes::at
                || current_char == CharacterCodes::hash
            {
                return false;
            }
        } else {
            return false;
        }
        true
    }

    pub(crate) fn get_named_members(&self, members: &SymbolTable) -> Vec<Rc<Symbol>> {
        members
            .iter()
            .filter(|(id, symbol)| self.is_named_member((*symbol).clone(), id))
            .map(|(_, symbol)| symbol.clone())
            .collect()
    }

    pub(crate) fn is_named_member(&self, member: Rc<Symbol>, escaped_name: &__String) -> bool {
        !self.is_reserved_member_name(escaped_name) && self.symbol_is_value(member)
    }

    pub(crate) fn set_structured_type_members<
        TType: ResolvableTypeInterface + ResolvedTypeInterface,
    >(
        &self,
        type_: &TType,
        members: Rc<RefCell<SymbolTable>>,
    ) /*-> BaseObjectType*/
    {
        type_.resolve(members.clone(), vec![]);
        if true {
            type_.set_properties(self.get_named_members(&*(*members).borrow()));
        }
        // type_
    }

    pub(crate) fn create_anonymous_type(
        &self,
        symbol: Rc<Symbol>,
        members: Rc<RefCell<SymbolTable>>,
    ) -> BaseObjectType {
        let type_ = self.create_object_type(ObjectFlags::Anonymous, symbol);
        self.set_structured_type_members(&type_, members);
        type_
    }

    pub(crate) fn symbol_to_string(
        &self,
        symbol: Rc<Symbol>,
        enclosing_declaration: Option<&Node>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(SymbolFormatFlags::AllowAnyNodeKind);
        let builder = if flags.intersects(SymbolFormatFlags::AllowAnyNodeKind) {
            NodeBuilder::symbol_to_expression
        } else {
            unimplemented!()
        };
        let symbol_to_string_worker = |writer: Rc<RefCell<dyn EmitTextWriter>>| {
            let entity = builder(
                &self.node_builder,
                self,
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                None,
            )
            .unwrap();
            let entity: Rc<Node> = entity.into();
            let mut printer = if false {
                unimplemented!()
            } else {
                create_printer(PrinterOptions {/*remove_comments: true*/})
            };
            let source_file = if let Some(enclosing_declaration) = enclosing_declaration {
                Some(get_source_file_of_node(enclosing_declaration))
            } else {
                None
            };
            printer.write_node(EmitHint::Unspecified, &*entity, source_file, writer);
            // writer
        };
        if let Some(writer) = writer {
            unimplemented!()
        } else {
            using_single_line_string_writer(symbol_to_string_worker)
        }
    }

    pub(crate) fn type_to_string(&self, type_: Rc<Type>) -> String {
        let writer = Rc::new(RefCell::new(create_text_writer("")));
        let type_node =
            self.node_builder
                .type_to_type_node(self, type_, Some(&*(*writer).borrow()));
        let type_node: Rc<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node.into(),
        };
        let options = PrinterOptions {};
        let mut printer = create_printer(options);
        let source_file: Option<Rc<SourceFile>> = if false { unimplemented!() } else { None };
        printer.write_node(
            EmitHint::Unspecified,
            &*type_node,
            source_file,
            writer.clone(),
        );
        let result = (*writer).borrow().get_text();

        result
    }

    pub(crate) fn get_type_names_for_error_display(
        &self,
        left: Rc<Type>,
        right: Rc<Type>,
    ) -> (String, String) {
        let left_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(left)
        };
        let right_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(right)
        };
        (left_str, right_str)
    }

    pub(crate) fn get_type_name_for_error_display(&self, type_: Rc<Type>) -> String {
        self.type_to_string(type_)
    }
}
