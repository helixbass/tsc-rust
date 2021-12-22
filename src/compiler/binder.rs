#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use parking_lot::RwLock;
use std::rc::Rc;

use crate::{
    Symbol, SymbolTable, SyntaxKind, VariableDeclaration, __String, append_if_unique,
    create_symbol_table, for_each, for_each_child, get_escaped_text_of_identifier_or_literal,
    get_name_of_declaration, is_binding_pattern, is_property_name_literal, object_allocator,
    set_parent, set_value_declaration, Expression, ExpressionStatement, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, Statement, SymbolFlags,
};

bitflags! {
    struct ContainerFlags: u32 {
        const None = 0;

        const IsContainer = 1 << 0;

        const IsControlFlowContainer = 1 << 2;

        const HasLocals = 1 << 5;
    }
}

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: Rc<Node>) {
    // binder.call(file);
    create_binder().call(file);
}

#[allow(non_snake_case)]
struct BinderType {
    file: RwLock<Option<Rc</*SourceFile*/ Node>>>,
    parent: RwLock<Option<Rc<Node>>>,
    container: RwLock<Option<Rc<Node>>>,
    Symbol: RwLock<Option<fn(SymbolFlags, __String) -> Symbol>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: RwLock::new(None),
        parent: RwLock::new(None),
        container: RwLock::new(None),
        Symbol: RwLock::new(None),
    }
}

impl BinderType {
    fn call(&self, f: Rc<Node>) {
        self.bind_source_file(f);
    }

    fn file(&self) -> Rc<Node> {
        self.file.try_read().unwrap().as_ref().unwrap().clone()
    }

    fn set_file(&self, file: Option<Rc<Node>>) {
        *self.file.try_write().unwrap() = file;
    }

    fn parent(&self) -> Rc<Node> {
        self.parent.try_read().unwrap().as_ref().unwrap().clone()
    }

    fn set_parent(&self, parent: Option<Rc<Node>>) {
        *self.parent.try_write().unwrap() = parent;
    }

    fn container(&self) -> Rc<Node> {
        self.container.try_read().unwrap().as_ref().unwrap().clone()
    }

    fn maybe_container(&self) -> Option<Rc<Node>> {
        self.container
            .try_read()
            .unwrap()
            .as_ref()
            .map(Clone::clone)
    }

    fn set_container(&self, container: Option<Rc<Node>>) {
        *self.container.try_write().unwrap() = container;
    }

    #[allow(non_snake_case)]
    fn Symbol(&self) -> fn(SymbolFlags, __String) -> Symbol {
        self.Symbol.try_read().unwrap().unwrap()
    }

    #[allow(non_snake_case)]
    fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> Symbol) {
        *self.Symbol.try_write().unwrap() = Some(Symbol);
    }

    fn bind_source_file(&self, f: Rc<Node>) {
        self.set_file(Some(f.clone()));

        self.set_Symbol(object_allocator.get_symbol_constructor());

        if true {
            self.bind(Some(self.file()));
        }

        self.set_file(None);
        self.set_parent(None);
        self.set_container(None);
    }

    fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        self.Symbol()(flags, name)
    }

    fn add_declaration_to_symbol(
        &self,
        symbol: Rc<Symbol>,
        node: Rc<Node /*Declaration*/>,
        symbol_flags: SymbolFlags,
    ) {
        symbol.set_flags(symbol.flags() | symbol_flags);

        node.set_symbol(symbol.clone());
        let declarations = {
            append_if_unique(
                symbol.maybe_declarations().as_ref().map(|vec| vec.clone()),
                node.clone(),
            )
        };
        symbol.set_declarations(declarations);

        if symbol_flags.intersects(SymbolFlags::Value) {
            set_value_declaration(&*symbol, node);
        }
    }

    fn get_declaration_name(&self, node: Rc<Node>) -> Option<__String> {
        let name = get_name_of_declaration(node);
        if let Some(name) = name {
            return if is_property_name_literal(&*name) {
                Some(get_escaped_text_of_identifier_or_literal(name.clone()))
            } else {
                None
            };
        }
        unimplemented!()
    }

    fn declare_symbol(
        &self,
        symbol_table: &mut SymbolTable,
        node: Rc<Node /*Declaration*/>,
        includes: SymbolFlags,
    ) -> Rc<Symbol> {
        let name = self.get_declaration_name(node.clone());

        let mut symbol = None;
        match name {
            None => unimplemented!(),
            Some(name) => {
                if true {
                    symbol = Some(Rc::new(self.create_symbol(SymbolFlags::None, name.clone())));
                    symbol_table.insert(name, symbol.as_ref().unwrap().clone());
                }
            }
        }
        let symbol = symbol.unwrap();

        self.add_declaration_to_symbol(symbol.clone(), node, includes);

        symbol
    }

    fn bind_container(&self, node: Rc<Node>, container_flags: ContainerFlags) {
        let save_container = self.maybe_container();

        if container_flags.intersects(ContainerFlags::IsContainer) {
            self.set_container(Some(node.clone()));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container().set_locals(create_symbol_table());
            }
        }

        if false {
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
    }

    fn bind_each_functions_first(&self, nodes: &NodeArray) {
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    fn bind_each(&self, nodes: &NodeArray) {
        for_each(nodes, |node, _| {
            self.bind(Some(node.clone()));
            Option::<()>::None
        });
    }

    fn bind_each_callback<TNodeCallback: FnMut(Rc<Node>)>(
        nodes: &NodeArray,
        mut bind_function: TNodeCallback,
    ) {
        for_each(nodes, |node, _| {
            bind_function(node.clone());
            Option::<()>::None
        });
    }

    fn bind_each_child(&self, node: Rc<Node>) {
        for_each_child(node, |node| self.bind(node), |nodes| self.bind_each(nodes));
    }

    fn bind_children(&self, node: Rc<Node>) {
        match &*node {
            Node::Statement(statement) => match statement {
                Statement::ExpressionStatement(expression_statement) => {
                    self.bind_expression_statement(expression_statement);
                }
                _ => {
                    self.bind_each_child(node);
                }
            },
            Node::Expression(expression) => match expression {
                Expression::PrefixUnaryExpression(_) => {
                    self.bind_prefix_unary_expression_flow(node);
                }
                Expression::BinaryExpression(_) => unimplemented!(),
                _ => {
                    self.bind_each_child(node);
                }
            },
            Node::SourceFile(source_file) => {
                self.bind_each_functions_first(&source_file.statements);
            }
            Node::VariableDeclarationList(_) => {
                self.bind_each_child(node);
            }
            Node::VariableDeclaration(_) => {
                self.bind_variable_declaration_flow(node);
            }
            Node::BaseNode(_) => panic!("Didn't expect to bind BaseNode?"),
            _ => unimplemented!(),
        };
    }

    fn bind_expression_statement(&self, node: &ExpressionStatement) {
        self.bind(Some(node.expression.clone()));
    }

    fn bind_prefix_unary_expression_flow(&self, node: Rc<Node>) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    fn bind_variable_declaration_flow(&self, node: Rc<Node /*VariableDeclaration*/>) {
        self.bind_each_child(node);
    }

    fn get_container_flags(&self, node: Rc<Node>) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::SourceFile => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals;
            }
            _ => (),
        }

        ContainerFlags::None
    }

    fn declare_symbol_and_add_to_symbol_table(
        &self,
        node: Rc<Node /*Declaration*/>,
        symbol_flags: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::SourceFile => Some(self.declare_source_file_member(node, symbol_flags)),
            _ => unimplemented!(),
        }
    }

    fn declare_source_file_member(
        &self,
        node: Rc<Node /*Declaration*/>,
        symbol_flags: SymbolFlags,
    ) -> Rc<Symbol> {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol(&mut *self.file().locals(), node, symbol_flags)
        }
    }

    fn bind(&self, node: Option<Rc<Node>>) {
        let node = match node.as_ref() {
            None => {
                return;
            }
            Some(node) => node.clone(),
        };
        set_parent(
            &*node,
            match self.parent.try_read().unwrap().as_ref() {
                None => None,
                Some(parent) => Some(parent.clone()),
            },
        );

        self.bind_worker(node.clone());

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = match *self.parent.try_read().unwrap() {
                None => None,
                Some(ref rc_node) => Some(rc_node.clone()),
            };
            self.set_parent(Some(node.clone()));
            let container_flags = self.get_container_flags(node.clone());
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    fn bind_worker(&self, node: Rc<Node>) {
        match &*node {
            Node::VariableDeclaration(variable_declaration) => {
                return self.bind_variable_declaration_or_binding_element(
                    variable_declaration,
                    node.clone(),
                );
            }
            _ => (),
        }
    }

    fn bind_variable_declaration_or_binding_element(
        &self,
        node: &VariableDeclaration,
        wrapper: Rc<Node>,
    ) {
        if !is_binding_pattern(&*node.name()) {
            if false {
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    wrapper,
                    SymbolFlags::FunctionScopedVariable,
                );
            }
        }
    }
}
