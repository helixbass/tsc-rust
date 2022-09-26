use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    file_extension_is_one_of, for_each_child_bool, Debug_, DiagnosticMessage, Extension,
    IncrementalParserSyntaxCursorReparseTopLevelAwait, IncrementalParserType, Node, NodeArray,
    NodeInterface, ParserType, ReadonlyPragmaMap, ReadonlyTextRange, SyntaxKind,
};

impl IncrementalParserType {
    pub fn create_syntax_cursor(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> IncrementalParserSyntaxCursor {
        IncrementalParserSyntaxCursor::create(source_file.node_wrapper())
    }
}

pub trait IncrementalParserSyntaxCursorInterface {
    fn current_node(&self, parser: &ParserType, position: usize) -> Option<Rc<Node>>;
}

pub enum IncrementalParserSyntaxCursor {
    Created(IncrementalParserSyntaxCursorCreated),
    ReparseTopLevelAwait(IncrementalParserSyntaxCursorReparseTopLevelAwait),
}

impl IncrementalParserSyntaxCursor {
    pub fn create(source_file: Rc<Node>) -> Self {
        Self::Created(IncrementalParserSyntaxCursorCreated::new(source_file))
    }
}

impl IncrementalParserSyntaxCursorInterface for IncrementalParserSyntaxCursor {
    fn current_node(&self, parser: &ParserType, position: usize) -> Option<Rc<Node>> {
        match self {
            IncrementalParserSyntaxCursor::Created(incremental_parser_syntax_cursor) => {
                incremental_parser_syntax_cursor.current_node(parser, position)
            }
            IncrementalParserSyntaxCursor::ReparseTopLevelAwait(
                incremental_parser_syntax_cursor,
            ) => incremental_parser_syntax_cursor.current_node(parser, position),
        }
    }
}

pub struct IncrementalParserSyntaxCursorCreated {
    source_file: Rc<Node /*SourceFile*/>,
    current_array: RefCell<Option<NodeArray>>,
    current_array_index: Cell<Option<usize>>,
    current: RefCell<Option<Rc<Node>>>,
    last_queried_position: Cell<Option<usize>>,
}

impl IncrementalParserSyntaxCursorCreated {
    pub fn new(source_file: Rc<Node>) -> Self {
        let source_file_as_source_file = source_file.as_source_file();
        let current_array = &source_file_as_source_file.statements;
        let current_array_index = 0;

        Debug_.assert(current_array_index < current_array.len(), None);
        Self {
            source_file: source_file.clone(),
            current_array: RefCell::new(Some(current_array.clone())),
            current_array_index: Cell::new(Some(current_array_index)),
            current: RefCell::new(Some(current_array[current_array_index].clone())),
            last_queried_position: Cell::new(None), /*InvalidPosition::Value*/
        }
    }

    fn current_array(&self) -> Ref<NodeArray> {
        Ref::map(self.current_array.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_current_array(&self, current_array: Option<NodeArray>) {
        *self.current_array.borrow_mut() = current_array;
    }

    fn current_array_index(&self) -> usize {
        self.current_array_index.get().unwrap()
    }

    fn set_current_array_index(&self, current_array_index: Option<usize>) {
        self.current_array_index.set(current_array_index)
    }

    fn maybe_current(&self) -> Option<Rc<Node>> {
        self.current.borrow().as_ref().map(|option| option.clone())
    }

    fn current(&self) -> Rc<Node> {
        self.current.borrow().clone().unwrap()
    }

    fn set_current(&self, current: Option<Rc<Node>>) {
        *self.current.borrow_mut() = current;
    }

    fn maybe_last_queried_position(&self) -> Option<usize> {
        self.last_queried_position.get()
    }

    fn set_last_queried_position(&self, last_queried_position: Option<usize>) {
        self.last_queried_position.set(last_queried_position)
    }

    fn find_highest_list_element_that_starts_at_position(&self, position: usize) {
        self.set_current_array(None);
        self.set_current_array_index(None /*InvalidPosition::Value*/);
        self.set_current(None);

        let position_as_isize = position.try_into().unwrap();

        for_each_child_bool(
            &self.source_file,
            |node: &Node| self.visit_node(position_as_isize, node),
            Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
        );
    }

    fn visit_node(&self, position_as_isize: isize, node: &Node) -> bool {
        if position_as_isize >= node.pos() && position_as_isize < node.end() {
            for_each_child_bool(
                node,
                |node: &Node| self.visit_node(position_as_isize, node),
                Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
            );
        }

        true
    }

    fn visit_array(&self, position_as_isize: isize, array: &NodeArray) -> bool {
        if position_as_isize >= array.pos() && position_as_isize < array.end() {
            for (i, child) in array.iter().enumerate() {
                // if (child) {
                if child.pos() == position_as_isize {
                    self.set_current_array(Some(array.clone()));
                    self.set_current_array_index(Some(i));
                    self.set_current(Some(child.clone()));
                    return true;
                } else {
                    if child.pos() < position_as_isize && position_as_isize < child.end() {
                        for_each_child_bool(
                            child,
                            |node: &Node| self.visit_node(position_as_isize, node),
                            Some(|array: &NodeArray| self.visit_array(position_as_isize, array)),
                        );
                        return true;
                    }
                }
                // }
            }
        }

        false
    }
}

impl IncrementalParserSyntaxCursorInterface for IncrementalParserSyntaxCursorCreated {
    fn current_node(&self, _parser: &ParserType, position: usize) -> Option<Rc<Node>> {
        let position_as_isize: isize = position.try_into().unwrap();
        if match self.maybe_last_queried_position() {
            None => true,
            Some(last_queried_position) => position != last_queried_position,
        } {
            if let Some(current) = self.maybe_current() {
                if current.end() == position_as_isize
                    && self.current_array_index() < (self.current_array().len() - 1)
                {
                    self.set_current_array_index(Some(self.current_array_index() + 1));
                    self.set_current(
                        self.current_array()
                            .get(self.current_array_index())
                            .map(Clone::clone),
                    );
                }
            }

            if match self.maybe_current() {
                None => true,
                Some(current) => current.pos() != position_as_isize,
            } {
                self.find_highest_list_element_that_starts_at_position(position);
            }
        }

        self.set_last_queried_position(Some(position));

        Debug_.assert(
            match self.maybe_current() {
                None => true,
                Some(current) => current.pos() == position_as_isize,
            },
            None,
        );
        self.maybe_current()
    }
}

pub(crate) fn is_declaration_file_name(file_name: &str) -> bool {
    file_extension_is_one_of(
        file_name,
        &vec![
            Extension::Dts.to_str(),
            Extension::Dmts.to_str(),
            Extension::Dcts.to_str(),
        ],
    )
}

pub(crate) trait PragmaContext {
    fn maybe_pragmas(&self) -> RefMut<Option<ReadonlyPragmaMap>>;
}

pub(crate) fn process_comment_pragmas<TContext: PragmaContext>(
    context: &TContext,
    source_text: &str,
) {
    // TODO
    *context.maybe_pragmas() = Some(HashMap::new());
    // TODO
}

pub(crate) fn process_pragmas_into_fields<
    TContext: PragmaContext,
    TReportDiagnostic: FnMut(isize, isize, &DiagnosticMessage),
>(
    context: &TContext,
    report_diagnostic: TReportDiagnostic,
) {
}

pub(crate) fn tag_names_are_equivalent(
    lhs: &Node, /*JsxTagNameExpression*/
    rhs: &Node, /*JsxTagNameExpression*/
) -> bool {
    if lhs.kind() != rhs.kind() {
        return false;
    }

    if lhs.kind() == SyntaxKind::Identifier {
        return lhs.as_identifier().escaped_text == rhs.as_identifier().escaped_text;
    }

    if lhs.kind() == SyntaxKind::ThisKeyword {
        return true;
    }

    let lhs_as_property_access_expression = lhs.as_property_access_expression();
    let rhs_as_property_access_expression = rhs.as_property_access_expression();
    lhs_as_property_access_expression
        .name
        .as_member_name()
        .escaped_text()
        == rhs_as_property_access_expression
            .name
            .as_member_name()
            .escaped_text()
        && tag_names_are_equivalent(
            &lhs_as_property_access_expression.expression,
            &rhs_as_property_access_expression.expression,
        )
}
