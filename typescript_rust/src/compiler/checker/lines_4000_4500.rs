use gc::{Gc, GcCell};
use std::collections::HashMap;
use std::ptr;
use std::{borrow::Borrow, io};

use super::{get_node_id, get_symbol_id, typeof_eq_facts};
use crate::{
    concatenate, create_symbol_table, filter, find_ancestor, for_each_entry,
    get_declaration_of_kind, get_source_file_of_node, is_ambient_module, is_external_module,
    is_external_module_import_equals_declaration, is_external_or_common_js_module, is_in_js_file,
    is_namespace_reexport_declaration, is_umd_export_symbol, length, maybe_for_each,
    node_is_present, push_if_unique_gc, some, try_for_each_entry, try_maybe_for_each,
    BaseInterfaceType, BaseIntrinsicType, BaseObjectType, BaseType, CharacterCodes,
    FunctionLikeDeclarationInterface, IndexInfo, InternalSymbolName, Node, NodeInterface,
    ObjectFlags, OptionTry, ResolvableTypeInterface, ResolvedTypeInterface, Signature,
    SignatureFlags, Symbol, SymbolAccessibility, SymbolAccessibilityResult, SymbolFlags, SymbolId,
    SymbolInterface, SymbolTable, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeParameter,
};

impl TypeChecker {
    pub(super) fn get_alias_for_symbol_in_container(
        &self,
        container: &Symbol,
        symbol: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        if matches!(
            self.get_parent_of_symbol(symbol)?,
            Some(parent) if ptr::eq(container, &*parent)
        ) {
            return Ok(Some(symbol.symbol_wrapper()));
        }
        let export_equals = container.maybe_exports().as_ref().and_then(|exports| {
            (**exports)
                .borrow()
                .get(InternalSymbolName::ExportEquals)
                .cloned()
        });
        if matches!(export_equals, Some(export_equals) if self.get_symbol_if_same_reference(&export_equals, symbol)?.is_some())
        {
            return Ok(Some(container.symbol_wrapper()));
        }
        let exports = self.get_exports_of_symbol(container)?;
        let exports = (*exports).borrow();
        let quick = exports.get(symbol.escaped_name());
        if let Some(quick) = quick {
            if self.get_symbol_if_same_reference(quick, symbol)?.is_some() {
                return Ok(Some(quick.clone()));
            }
        }
        try_for_each_entry(&*exports, |exported: &Gc<Symbol>, _| {
            if self
                .get_symbol_if_same_reference(exported, symbol)?
                .is_some()
            {
                return Ok(Some(exported.clone()));
            }
            Ok(None)
        })
    }

    pub(super) fn get_symbol_if_same_reference(
        &self,
        s1: &Symbol,
        s2: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        if Gc::ptr_eq(
            &self
                .get_merged_symbol(self.resolve_symbol(self.get_merged_symbol(Some(s1)), None)?)
                .unwrap(),
            &self
                .get_merged_symbol(self.resolve_symbol(self.get_merged_symbol(Some(s2)), None)?)
                .unwrap(),
        ) {
            return Ok(Some(s1.symbol_wrapper()));
        }
        Ok(None)
    }

    pub(super) fn get_export_symbol_of_value_symbol_if_exported(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> Option<Gc<Symbol>> {
        self.get_merged_symbol(symbol.and_then(|symbol| {
            let symbol = symbol.borrow();
            if symbol.flags().intersects(SymbolFlags::ExportValue) {
                symbol.maybe_export_symbol()
            } else {
                Some(symbol.symbol_wrapper())
            }
        }))
    }

    pub(super) fn symbol_is_value(&self, symbol: &Symbol) -> io::Result<bool> {
        Ok(symbol.flags().intersects(SymbolFlags::Value)
            || symbol.flags().intersects(SymbolFlags::Alias)
                && self
                    .resolve_alias(symbol)?
                    .flags()
                    .intersects(SymbolFlags::Value)
                && self.get_type_only_alias_declaration(symbol).is_none())
    }

    pub(super) fn find_constructor_declaration(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
    ) -> Option<Gc<Node /*ConstructorDeclaration*/>> {
        let members = node.as_class_like_declaration().members();
        for member in &members {
            if member.kind() == SyntaxKind::Constructor
                && node_is_present(member.as_constructor_declaration().maybe_body())
            {
                return Some(member.clone());
            }
        }
        None
    }

    pub(super) fn create_type(&self, flags: TypeFlags) -> BaseType {
        let mut result = (self.Type)(flags);
        self.increment_type_count();
        result.id = Some(self.type_count());
        // if (produceDiagnostics) {
        //     tracing?.recordType(result);
        // }
        result
    }

    pub(super) fn create_origin_type(&self, flags: TypeFlags) -> BaseType {
        (self.Type)(flags)
    }

    pub(super) fn create_intrinsic_type(
        &self,
        kind: TypeFlags,
        intrinsic_name: &str,
        object_flags: Option<ObjectFlags>,
    ) -> BaseIntrinsicType {
        let object_flags = object_flags.unwrap_or(ObjectFlags::None);
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_, intrinsic_name.to_owned(), object_flags);
        type_
    }

    pub(super) fn create_object_type<TSymbol: Borrow<Symbol>>(
        &self,
        object_flags: ObjectFlags,
        symbol: Option<TSymbol>,
    ) -> BaseObjectType {
        let type_ = self.create_type(TypeFlags::Object);
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        let type_ = BaseObjectType::new(type_, object_flags);
        type_
    }

    pub(super) fn create_typeof_type(&self) -> io::Result<Gc<Type>> {
        self.get_union_type(
            &typeof_eq_facts
                .keys()
                .map(|key| -> Gc<Type> { self.get_string_literal_type(key).into() })
                .collect::<Vec<_>>(),
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )
    }

    pub(super) fn create_type_parameter(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> TypeParameter {
        let type_ = self.create_type(TypeFlags::TypeParameter);
        if let Some(symbol) = symbol {
            let symbol = symbol.borrow();
            type_.set_symbol(Some(symbol.symbol_wrapper()));
        }
        let type_ = TypeParameter::new(type_);
        type_
    }

    pub(super) fn is_reserved_member_name(&self, name: &str /*__String*/) -> bool {
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

    pub(super) fn get_named_members(&self, members: &SymbolTable) -> io::Result<Vec<Gc<Symbol>>> {
        let mut results = vec![];
        for (id, symbol) in members {
            if self.is_named_member(symbol, id)? {
                results.push(symbol.clone());
            }
        }
        Ok(results)
    }

    pub(super) fn is_named_member(
        &self,
        member: &Symbol,
        escaped_name: &str, /*__String*/
    ) -> io::Result<bool> {
        Ok(!self.is_reserved_member_name(escaped_name) && self.symbol_is_value(member)?)
    }

    pub(super) fn get_named_or_index_signature_members(
        &self,
        members: &SymbolTable,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        let result = self.get_named_members(members)?;
        let index = self.get_index_symbol_from_symbol_table(members);
        Ok(if let Some(index) = index {
            concatenate(result, vec![index])
        } else {
            result
        })
    }

    pub(super) fn set_structured_type_members(
        &self,
        type_: &(impl ResolvableTypeInterface + ResolvedTypeInterface),
        members: Gc<GcCell<SymbolTable>>,
        call_signatures: Vec<Gc<Signature>>,
        construct_signatures: Vec<Gc<Signature>>,
        index_infos: Vec<Gc<IndexInfo>>,
    ) -> io::Result<()> /*-> BaseObjectType*/ {
        type_.resolve(
            members.clone(),
            vec![].into(),
            call_signatures,
            construct_signatures,
            index_infos,
        );
        if !Gc::ptr_eq(&members, &self.empty_symbols()) {
            type_.set_properties(self.get_named_members(&(*members).borrow())?.into());
        }
        // type_

        Ok(())
    }

    // extracted this because it's easy to do BaseObjectType.into() -> Gc<Type> to incorrectly wrap
    // this return type (should be wrapped in a BaseInterfaceType) (so only "knowing" consumers
    // should call this instead of the "default" .create_anonymous_type() which does the correct
    // wrapping for you)
    pub(super) fn create_anonymous_type_returning_base_object_type(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
        members: Gc<GcCell<SymbolTable>>,
        call_signatures: Vec<Gc<Signature>>,
        construct_signatures: Vec<Gc<Signature>>,
        index_infos: Vec<Gc<IndexInfo>>,
    ) -> io::Result<BaseObjectType> {
        let type_ = self.create_object_type(ObjectFlags::Anonymous, symbol);
        self.set_structured_type_members(
            &type_,
            members,
            call_signatures,
            construct_signatures,
            index_infos,
        )?;
        Ok(type_)
    }

    pub(super) fn create_anonymous_type(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
        members: Gc<GcCell<SymbolTable>>,
        call_signatures: Vec<Gc<Signature>>,
        construct_signatures: Vec<Gc<Signature>>,
        index_infos: Vec<Gc<IndexInfo>>,
    ) -> io::Result<Gc<Type>> {
        Ok(BaseInterfaceType::new(
            self.create_anonymous_type_returning_base_object_type(
                symbol,
                members,
                call_signatures,
                construct_signatures,
                index_infos,
            )?,
            None,
            None,
            None,
            None,
        )
        .into())
    }

    pub(super) fn get_resolved_type_without_abstract_construct_signatures(
        &self,
        type_: &Type, /*ResolvedType*/
    ) -> io::Result<Gc<Type>> {
        let type_as_resolved_type = type_.as_resolved_type();
        let type_construct_signatures = type_as_resolved_type.construct_signatures();
        if type_construct_signatures.is_empty() {
            return Ok(type_.type_wrapper());
        }
        if let Some(type_object_type_without_abstract_construct_signatures) =
            type_as_resolved_type.maybe_object_type_without_abstract_construct_signatures()
        {
            return Ok(type_object_type_without_abstract_construct_signatures);
        }
        let construct_signatures =
            filter(&*type_construct_signatures, |signature: &Gc<Signature>| {
                !signature.flags.intersects(SignatureFlags::Abstract)
            });
        if type_construct_signatures.len() == construct_signatures.len() {
            return Ok(type_.type_wrapper());
        }
        let type_copy = self.create_anonymous_type(
            type_.maybe_symbol(),
            type_as_resolved_type.members(),
            type_as_resolved_type.call_signatures().clone(),
            construct_signatures,
            type_as_resolved_type.index_infos().clone(),
        )?;
        type_as_resolved_type
            .set_object_type_without_abstract_construct_signatures(Some(type_copy.clone()));
        type_copy
            .as_resolved_type()
            .set_object_type_without_abstract_construct_signatures(Some(type_copy.clone()));
        Ok(type_copy)
    }

    pub(super) fn for_each_symbol_table_in_scope<TReturn>(
        &self,
        enclosing_declaration: Option<impl Borrow<Node>>,
        mut callback: impl FnMut(
            Gc<GcCell<SymbolTable>>,
            Option<bool>,
            Option<bool>,
            Option<&Node>,
        ) -> Option<TReturn>,
    ) -> Option<TReturn> {
        self.try_for_each_symbol_table_in_scope(
            enclosing_declaration,
            |a: Gc<GcCell<SymbolTable>>, b: Option<bool>, c: Option<bool>, d: Option<&Node>| {
                Ok(callback(a, b, c, d))
            },
        )
        .unwrap()
    }

    pub(super) fn try_for_each_symbol_table_in_scope<TReturn>(
        &self,
        enclosing_declaration: Option<impl Borrow<Node>>,
        mut callback: impl FnMut(
            Gc<GcCell<SymbolTable>>,
            Option<bool>,
            Option<bool>,
            Option<&Node>,
        ) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
        let mut result: Option<TReturn>;
        let mut location: Option<Gc<Node>> = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        while let Some(location_unwrapped) = location {
            if let Some(location_locals) = location_unwrapped.maybe_locals().as_ref() {
                if !self.is_global_source_file(&location_unwrapped) {
                    result = callback(
                        location_locals.clone(),
                        None,
                        Some(true),
                        Some(&*location_unwrapped),
                    )?;
                    if result.is_some() {
                        return Ok(result);
                    }
                }
            }
            match location_unwrapped.kind() {
                SyntaxKind::SourceFile | SyntaxKind::ModuleDeclaration => {
                    if !(location_unwrapped.kind() == SyntaxKind::SourceFile
                        && !is_external_or_common_js_module(&location_unwrapped))
                    {
                        let sym = self.get_symbol_of_node(&location_unwrapped)?;
                        result = callback(
                            sym.and_then(|sym| sym.maybe_exports().clone())
                                .unwrap_or_else(|| self.empty_symbols()),
                            None,
                            Some(true),
                            Some(&*location_unwrapped),
                        )?;
                        if result.is_some() {
                            return Ok(result);
                        }
                    }
                }
                SyntaxKind::ClassDeclaration
                | SyntaxKind::ClassExpression
                | SyntaxKind::InterfaceDeclaration => {
                    let mut table: Option<SymbolTable> = None;
                    for (key, member_symbol) in &*(*self
                        .get_symbol_of_node(&location_unwrapped)?
                        .unwrap()
                        .maybe_members()
                        .clone()
                        .unwrap_or_else(|| self.empty_symbols()))
                    .borrow()
                    {
                        if member_symbol
                            .flags()
                            .intersects(SymbolFlags::Type & !SymbolFlags::Assignment)
                        {
                            if table.is_none() {
                                table = Some(create_symbol_table(Option::<&[Gc<Symbol>]>::None));
                            }
                            table
                                .as_mut()
                                .unwrap()
                                .insert(key.clone(), member_symbol.clone());
                        }
                    }
                    if let Some(table) = table {
                        result = callback(
                            Gc::new(GcCell::new(table)),
                            None,
                            Some(false),
                            Some(&*location_unwrapped),
                        )?;
                        if result.is_some() {
                            return Ok(result);
                        }
                    }
                }
                _ => (),
            }

            location = location_unwrapped.maybe_parent();
        }

        callback(self.globals_rc(), None, Some(true), None)
    }

    pub(super) fn get_qualified_left_meaning(&self, right_meaning: SymbolFlags) -> SymbolFlags {
        if right_meaning == SymbolFlags::Value {
            SymbolFlags::Value
        } else {
            SymbolFlags::Namespace
        }
    }

    pub(super) fn get_accessible_symbol_chain(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: SymbolFlags,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: Option<
            &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        >,
    ) -> io::Result<Option<Vec<Gc<Symbol>>>> {
        let mut visited_symbol_tables_map_default = HashMap::new();
        let visited_symbol_tables_map =
            visited_symbol_tables_map.unwrap_or(&mut visited_symbol_tables_map_default);
        if symbol.is_none() {
            return Ok(None);
        }
        let symbol = symbol.unwrap();
        let symbol = symbol.borrow();
        if self.is_property_or_method_declaration_symbol(symbol) {
            return Ok(None);
        }
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().accessible_chain_cache.is_none() {
            links.borrow_mut().accessible_chain_cache = Some(HashMap::new());
        }
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let first_relevant_location = self
            .for_each_symbol_table_in_scope(enclosing_declaration.as_deref(), |_, _, _, node| {
                node.map(|node| node.node_wrapper())
            });
        let key = format!(
            "{}|{}|{}",
            if use_only_external_aliasing { 0 } else { 1 },
            if let Some(first_relevant_location) = first_relevant_location.as_ref() {
                get_node_id(first_relevant_location).to_string()
            } else {
                "undefined".to_owned()
            },
            meaning.bits()
        );
        {
            let mut links = (*links).borrow_mut();
            let cache = links.accessible_chain_cache.as_mut().unwrap();
            if cache.contains_key(&key) {
                return Ok(cache.get(&key).unwrap().clone());
            }
        }

        let id = get_symbol_id(symbol);
        if !visited_symbol_tables_map.contains_key(&id) {
            visited_symbol_tables_map.insert(id, Gc::new(GcCell::new(vec![])));
        }
        let visited_symbol_tables = visited_symbol_tables_map.get(&id).unwrap().clone();
        let result = self.try_for_each_symbol_table_in_scope(
            enclosing_declaration.as_deref(),
            |symbols, ignore_qualification, is_local_name_lookup, _| {
                self.get_accessible_symbol_chain_from_symbol_table(
                    visited_symbol_tables.clone(),
                    meaning,
                    symbol,
                    enclosing_declaration.as_deref(),
                    use_only_external_aliasing,
                    visited_symbol_tables_map,
                    symbols,
                    ignore_qualification,
                    is_local_name_lookup,
                )
            },
        )?;
        (*links)
            .borrow_mut()
            .accessible_chain_cache
            .as_mut()
            .unwrap()
            .insert(key, result.clone());
        Ok(result)
    }

    pub(super) fn get_accessible_symbol_chain_from_symbol_table(
        &self,
        visited_symbol_tables: Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>,
        meaning: SymbolFlags,
        symbol: &Symbol,
        enclosing_declaration: Option<&Node>,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        symbols: Gc<GcCell<SymbolTable>>,
        ignore_qualification: Option<bool>,
        is_local_name_lookup: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Symbol>>>> {
        if !push_if_unique_gc(&mut visited_symbol_tables.borrow_mut(), &symbols) {
            return Ok(None);
        }

        let result = self.try_symbol_table(
            symbol,
            meaning,
            enclosing_declaration,
            use_only_external_aliasing,
            visited_symbol_tables_map,
            visited_symbol_tables.clone(),
            symbols,
            ignore_qualification,
            is_local_name_lookup,
        )?;
        visited_symbol_tables.borrow_mut().pop();
        Ok(result)
    }

    pub(super) fn can_qualify_symbol(
        &self,
        enclosing_declaration: Option<&Node>,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        symbol_from_symbol_table: &Symbol,
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
        Ok(
            !self.needs_qualification(symbol_from_symbol_table, enclosing_declaration, meaning)?
                || self
                    .get_accessible_symbol_chain(
                        symbol_from_symbol_table.maybe_parent(),
                        enclosing_declaration,
                        self.get_qualified_left_meaning(meaning),
                        use_only_external_aliasing,
                        Some(visited_symbol_tables_map),
                    )?
                    .is_some(),
        )
    }

    pub(super) fn is_accessible(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<&Node>,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        symbol_from_symbol_table: Option<impl Borrow<Symbol>>,
        resolved_alias_symbol: Option<impl Borrow<Symbol>>,
        ignore_qualification: Option<bool>,
    ) -> io::Result<bool> {
        let symbol_from_symbol_table = symbol_from_symbol_table
            .map(|symbol_from_symbol_table| symbol_from_symbol_table.borrow().symbol_wrapper());
        let resolved_alias_symbol = resolved_alias_symbol
            .map(|resolved_alias_symbol| resolved_alias_symbol.borrow().symbol_wrapper());
        Ok((matches!(
            resolved_alias_symbol
                .as_deref()
                .or_else(|| symbol_from_symbol_table.as_deref()),
            Some(value) if ptr::eq(
                symbol,
                value
            )
        ) || matches!(
            self
                .get_merged_symbol(
                    resolved_alias_symbol
                        .as_deref()
                        .or_else(|| symbol_from_symbol_table.as_deref()),
                ).as_ref(),
            Some(value) if Gc::ptr_eq(
                &self.get_merged_symbol(Some(symbol)).unwrap(),
                value,
            )
        )) && !some(
            symbol_from_symbol_table
                .as_ref()
                .unwrap()
                .maybe_declarations()
                .as_deref(),
            Some(|declaration: &Gc<Node>| {
                self.has_non_global_augmentation_external_module_symbol(declaration)
            }),
        ) && (matches!(ignore_qualification, Some(true))
            || self.can_qualify_symbol(
                enclosing_declaration,
                use_only_external_aliasing,
                visited_symbol_tables_map,
                &self.get_merged_symbol(symbol_from_symbol_table).unwrap(),
                meaning,
            )?))
    }

    pub(super) fn try_symbol_table(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<&Node>,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        visited_symbol_tables: Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>,
        symbols: Gc<GcCell<SymbolTable>>,
        ignore_qualification: Option<bool>,
        is_local_name_lookup: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Symbol>>>> {
        if self.is_accessible(
            symbol,
            meaning,
            enclosing_declaration,
            use_only_external_aliasing,
            visited_symbol_tables_map,
            (*symbols).borrow().get(symbol.escaped_name()).cloned(),
            Option::<&Symbol>::None,
            ignore_qualification,
        )? {
            return Ok(Some(vec![symbol.symbol_wrapper()]));
        }

        let result: Option<Vec<Gc<Symbol>>> = try_for_each_entry(
            &*(*symbols).borrow(),
            |symbol_from_symbol_table, _| -> io::Result<_> {
                if symbol_from_symbol_table
                    .flags()
                    .intersects(SymbolFlags::Alias)
                    && symbol_from_symbol_table.escaped_name() != InternalSymbolName::ExportEquals
                    && symbol_from_symbol_table.escaped_name() != InternalSymbolName::Default
                    && !(is_umd_export_symbol(Some(&**symbol_from_symbol_table))
                        && matches!(enclosing_declaration, Some(enclosing_declaration) if is_external_module(&get_source_file_of_node(enclosing_declaration))))
                    && (!use_only_external_aliasing
                        || some(
                            symbol_from_symbol_table.maybe_declarations().as_deref(),
                            Some(|declaration: &Gc<Node>| {
                                is_external_module_import_equals_declaration(declaration)
                            }),
                        ))
                    && if matches!(is_local_name_lookup, Some(true)) {
                        !some(
                            symbol_from_symbol_table.maybe_declarations().as_deref(),
                            Some(|declaration: &Gc<Node>| {
                                is_namespace_reexport_declaration(declaration)
                            }),
                        )
                    } else {
                        true
                    }
                    && (matches!(ignore_qualification, Some(true))
                        || get_declaration_of_kind(
                            symbol_from_symbol_table,
                            SyntaxKind::ExportSpecifier,
                        )
                        .is_none())
                {
                    let resolved_import_symbol = self.resolve_alias(symbol_from_symbol_table)?;
                    let candidate = self.get_candidate_list_for_symbol(
                        symbol,
                        meaning,
                        enclosing_declaration,
                        use_only_external_aliasing,
                        visited_symbol_tables_map,
                        visited_symbol_tables.clone(),
                        symbol_from_symbol_table,
                        &resolved_import_symbol,
                        ignore_qualification,
                    )?;
                    if candidate.is_some() {
                        return Ok(candidate);
                    }
                }
                if symbol_from_symbol_table.escaped_name() == symbol.escaped_name() {
                    if let Some(symbol_from_symbol_table_export_symbol) =
                        symbol_from_symbol_table.maybe_export_symbol()
                    {
                        if self.is_accessible(
                            symbol,
                            meaning,
                            enclosing_declaration,
                            use_only_external_aliasing,
                            visited_symbol_tables_map,
                            self.get_merged_symbol(Some(&*symbol_from_symbol_table_export_symbol)),
                            Option::<&Symbol>::None,
                            ignore_qualification,
                        )? {
                            return Ok(Some(vec![symbol.symbol_wrapper()]));
                        }
                    }
                }
                Ok(None)
            },
        )?;

        result.try_or_else(|| {
            Ok(if Gc::ptr_eq(&symbols, &self.globals_rc()) {
                self.get_candidate_list_for_symbol(
                    symbol,
                    meaning,
                    enclosing_declaration,
                    use_only_external_aliasing,
                    visited_symbol_tables_map,
                    visited_symbol_tables,
                    &self.global_this_symbol(),
                    &self.global_this_symbol(),
                    ignore_qualification,
                )?
            } else {
                None
            })
        })
    }

    pub(super) fn get_candidate_list_for_symbol(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<&Node>,
        use_only_external_aliasing: bool,
        visited_symbol_tables_map: &mut HashMap<SymbolId, Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>>,
        visited_symbol_tables: Gc<GcCell<Vec<Gc<GcCell<SymbolTable>>>>>,
        symbol_from_symbol_table: &Symbol,
        resolved_import_symbol: &Symbol,
        ignore_qualification: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Symbol>>>> {
        if self.is_accessible(
            symbol,
            meaning,
            enclosing_declaration,
            use_only_external_aliasing,
            visited_symbol_tables_map,
            Some(symbol_from_symbol_table),
            Some(resolved_import_symbol),
            ignore_qualification,
        )? {
            return Ok(Some(vec![symbol_from_symbol_table.symbol_wrapper()]));
        }

        let candidate_table = self.get_exports_of_symbol(resolved_import_symbol)?;
        let accessible_symbols_from_exports = /*candidateTable &&*/
            self.get_accessible_symbol_chain_from_symbol_table(
                visited_symbol_tables,
                meaning,
                symbol,
                enclosing_declaration,
                use_only_external_aliasing,
                visited_symbol_tables_map,
                candidate_table,
                Some(true),
                None,
            )?;
        if let Some(mut accessible_symbols_from_exports) = accessible_symbols_from_exports {
            if self.can_qualify_symbol(
                enclosing_declaration,
                use_only_external_aliasing,
                visited_symbol_tables_map,
                symbol_from_symbol_table,
                self.get_qualified_left_meaning(meaning),
            )? {
                let mut ret = vec![symbol_from_symbol_table.symbol_wrapper()];
                ret.append(&mut accessible_symbols_from_exports);
                return Ok(Some(ret));
            }
        }
        Ok(None)
    }

    pub(super) fn needs_qualification(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
        let mut qualify = false;
        self.try_for_each_symbol_table_in_scope(
            enclosing_declaration,
            |symbol_table, _, _, _| -> io::Result<_> {
                let symbol_from_symbol_table = self.get_merged_symbol(
                    (*symbol_table).borrow().get(symbol.escaped_name()).cloned(),
                );
                if symbol_from_symbol_table.is_none() {
                    return Ok(None);
                }
                let mut symbol_from_symbol_table = symbol_from_symbol_table.unwrap();
                if ptr::eq(&*symbol_from_symbol_table, symbol) {
                    return Ok(Some(()));
                }

                symbol_from_symbol_table = if symbol_from_symbol_table
                    .flags()
                    .intersects(SymbolFlags::Alias)
                    && get_declaration_of_kind(
                        &symbol_from_symbol_table,
                        SyntaxKind::ExportSpecifier,
                    )
                    .is_none()
                {
                    self.resolve_alias(&symbol_from_symbol_table)?
                } else {
                    symbol_from_symbol_table
                };
                if symbol_from_symbol_table.flags().intersects(meaning) {
                    qualify = true;
                    return Ok(Some(()));
                }

                Ok(None)
            },
        )?;

        Ok(qualify)
    }

    pub(super) fn is_property_or_method_declaration_symbol(&self, symbol: &Symbol) -> bool {
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
            if !symbol_declarations.is_empty() {
                for declaration in symbol_declarations {
                    match declaration.kind() {
                        SyntaxKind::PropertyDeclaration
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::GetAccessor
                        | SyntaxKind::SetAccessor => {
                            continue;
                        }
                        _ => {
                            return false;
                        }
                    }
                }
                return true;
            }
        }
        false
    }

    pub(super) fn is_type_symbol_accessible(
        &self,
        type_symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
    ) -> io::Result<bool> {
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let access = self.is_symbol_accessible_worker(
            Some(type_symbol),
            enclosing_declaration.as_deref(),
            SymbolFlags::Type,
            false,
            true,
        )?;
        Ok(access.accessibility == SymbolAccessibility::Accessible)
    }

    pub(super) fn is_value_symbol_accessible(
        &self,
        type_symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
    ) -> io::Result<bool> {
        let access = self.is_symbol_accessible_worker(
            Some(type_symbol),
            enclosing_declaration,
            SymbolFlags::Value,
            false,
            true,
        )?;
        Ok(access.accessibility == SymbolAccessibility::Accessible)
    }

    pub(super) fn is_symbol_accessible_by_flags(
        &self,
        type_symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: SymbolFlags,
    ) -> io::Result<bool> {
        let access = self.is_symbol_accessible_worker(
            Some(type_symbol),
            enclosing_declaration,
            flags,
            false,
            false,
        )?;
        Ok(access.accessibility == SymbolAccessibility::Accessible)
    }

    pub(super) fn is_any_symbol_accessible(
        &self,
        symbols: Option<&[Gc<Symbol>]>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        initial_symbol: &Symbol,
        meaning: SymbolFlags,
        should_compute_aliases_to_make_visible: bool,
        allow_modules: bool,
    ) -> io::Result<Option<SymbolAccessibilityResult>> {
        if length(symbols) == 0 {
            return Ok(None);
        }
        let symbols = symbols.unwrap();

        let mut had_accessible_chain: Option<Gc<Symbol>> = None;
        let mut early_module_bail = false;
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        for symbol in symbols {
            let accessible_symbol_chain = self.get_accessible_symbol_chain(
                Some(&**symbol),
                enclosing_declaration.as_deref(),
                meaning,
                false,
                None,
            )?;
            if let Some(accessible_symbol_chain) = accessible_symbol_chain {
                had_accessible_chain = Some(symbol.clone());
                let has_accessible_declarations = self.has_visible_declarations(
                    &accessible_symbol_chain[0],
                    should_compute_aliases_to_make_visible,
                );
                if let Some(has_accessible_declarations) = has_accessible_declarations {
                    return Ok(Some(
                        has_accessible_declarations.into_symbol_accessibility_result(),
                    ));
                }
            }
            if allow_modules {
                if some(
                    symbol.maybe_declarations().as_deref(),
                    Some(|declaration: &Gc<Node>| {
                        self.has_non_global_augmentation_external_module_symbol(declaration)
                    }),
                ) {
                    if should_compute_aliases_to_make_visible {
                        early_module_bail = true;
                        continue;
                    }
                    return Ok(Some(SymbolAccessibilityResult {
                        accessibility: SymbolAccessibility::Accessible,
                        aliases_to_make_visible: None,
                        error_symbol_name: None,
                        error_node: None,
                        error_module_name: None,
                    }));
                }
            }

            let containers =
                self.get_containers_of_symbol(symbol, enclosing_declaration.as_deref(), meaning)?;
            let parent_result = self.is_any_symbol_accessible(
                containers.as_deref(),
                enclosing_declaration.as_deref(),
                initial_symbol,
                if ptr::eq(initial_symbol, &**symbol) {
                    self.get_qualified_left_meaning(meaning)
                } else {
                    meaning
                },
                should_compute_aliases_to_make_visible,
                allow_modules,
            )?;
            if parent_result.is_some() {
                return Ok(parent_result);
            }
        }

        if early_module_bail {
            return Ok(Some(SymbolAccessibilityResult {
                accessibility: SymbolAccessibility::Accessible,
                aliases_to_make_visible: None,
                error_symbol_name: None,
                error_node: None,
                error_module_name: None,
            }));
        }

        if let Some(had_accessible_chain) = had_accessible_chain {
            return Ok(Some(SymbolAccessibilityResult {
                accessibility: SymbolAccessibility::NotAccessible,
                aliases_to_make_visible: None,
                error_symbol_name: Some(self.symbol_to_string_(
                    initial_symbol,
                    enclosing_declaration.as_deref(),
                    Some(meaning),
                    None,
                    None,
                )?),
                error_node: None,
                error_module_name: if !ptr::eq(&*had_accessible_chain, initial_symbol) {
                    Some(self.symbol_to_string_(
                        &had_accessible_chain,
                        enclosing_declaration.as_deref(),
                        Some(SymbolFlags::Namespace),
                        None,
                        None,
                    )?)
                } else {
                    None
                },
            }));
        }

        Ok(None)
    }

    pub(super) fn is_symbol_accessible(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: SymbolFlags,
        should_compute_aliases_to_make_visible: bool,
    ) -> io::Result<SymbolAccessibilityResult> {
        self.is_symbol_accessible_worker(
            symbol,
            enclosing_declaration,
            meaning,
            should_compute_aliases_to_make_visible,
            true,
        )
    }

    pub(super) fn is_symbol_accessible_worker(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: SymbolFlags,
        should_compute_aliases_to_make_visible: bool,
        allow_modules: bool,
    ) -> io::Result<SymbolAccessibilityResult> {
        if let Some(symbol) = symbol {
            let symbol = symbol.borrow();
            if let Some(enclosing_declaration) = enclosing_declaration {
                let enclosing_declaration = enclosing_declaration.borrow();
                let result = self.is_any_symbol_accessible(
                    Some(&vec![symbol.symbol_wrapper()]),
                    Some(enclosing_declaration),
                    symbol,
                    meaning,
                    should_compute_aliases_to_make_visible,
                    allow_modules,
                )?;
                if let Some(result) = result {
                    return Ok(result);
                }

                let symbol_external_module = try_maybe_for_each(
                    symbol.maybe_declarations().as_deref(),
                    |declaration: &Gc<Node>, _| self.get_external_module_container(declaration),
                )?;
                if let Some(symbol_external_module) = symbol_external_module {
                    let enclosing_external_module =
                        self.get_external_module_container(enclosing_declaration)?;
                    if !matches!(
                        enclosing_external_module,
                        Some(enclosing_external_module) if Gc::ptr_eq(
                            &symbol_external_module,
                            &enclosing_external_module
                        )
                    ) {
                        return Ok(SymbolAccessibilityResult {
                            accessibility: SymbolAccessibility::CannotBeNamed,
                            aliases_to_make_visible: None,
                            error_symbol_name: Some(self.symbol_to_string_(
                                symbol,
                                Some(enclosing_declaration),
                                Some(meaning),
                                None,
                                None,
                            )?),
                            error_module_name: Some(self.symbol_to_string_(
                                &symbol_external_module,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )?),
                            error_node: if is_in_js_file(Some(enclosing_declaration)) {
                                Some(enclosing_declaration.node_wrapper())
                            } else {
                                None
                            },
                        });
                    }
                }

                return Ok(SymbolAccessibilityResult {
                    accessibility: SymbolAccessibility::NotAccessible,
                    aliases_to_make_visible: None,
                    error_symbol_name: Some(self.symbol_to_string_(
                        symbol,
                        Some(enclosing_declaration),
                        Some(meaning),
                        None,
                        None,
                    )?),
                    error_node: None,
                    error_module_name: None,
                });
            }
        }

        Ok(SymbolAccessibilityResult {
            accessibility: SymbolAccessibility::Accessible,
            aliases_to_make_visible: None,
            error_symbol_name: None,
            error_node: None,
            error_module_name: None,
        })
    }

    pub(super) fn get_external_module_container(
        &self,
        declaration: &Node,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let node = find_ancestor(Some(declaration), |node| {
            self.has_external_module_symbol(node)
        });
        node.try_and_then(|node| self.get_symbol_of_node(&node))
    }

    pub(super) fn has_external_module_symbol(&self, declaration: &Node) -> bool {
        is_ambient_module(declaration)
            || declaration.kind() == SyntaxKind::SourceFile
                && is_external_or_common_js_module(declaration)
    }
}
