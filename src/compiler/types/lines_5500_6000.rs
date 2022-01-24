#![allow(non_upper_case_globals)]

use std::cell::{RefCell, RefMut};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::BitAndAssign;
use std::rc::{Rc, Weak};

use super::{BaseType, SourceFile, Symbol, SymbolTable, Type, TypeChecker};
use local_macros::{enum_unwrapped, type_type};

pub trait ResolvedTypeInterface {
    fn members(&self) -> Rc<RefCell<SymbolTable>>;
    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>>;
    fn set_properties(&self, properties: Vec<Rc<Symbol>>);
}

#[derive(Clone, Debug)]
#[type_type]
pub struct TypeParameter {
    _type: BaseType,
    pub constraint: RefCell<Option<Weak<Type>>>, // TODO: is it correct that this is weak?
    pub is_this_type: Option<bool>,
}

impl TypeParameter {
    pub fn new(base_type: BaseType) -> Self {
        Self {
            _type: base_type,
            constraint: RefCell::new(None),
            is_this_type: None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeMapper {
    Simple(TypeMapperSimple),
    Array(TypeMapperArray),
    Function(TypeMapperFunction),
    Composite(TypeMapperCompositeOrMerged),
    Merged(TypeMapperCompositeOrMerged),
}

#[derive(Clone, Debug)]
pub struct TypeMapperSimple {
    pub source: Rc<Type>, // TODO: should all of these be weak?
    pub target: Rc<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeMapperArray {
    pub sources: Vec<Rc<Type>>,
    pub targets: Option<Vec<Rc<Type>>>,
}

#[derive(Clone)]
pub struct TypeMapperFunction {
    pub func: fn(&TypeChecker, &Type) -> Rc<Type>,
}

impl fmt::Debug for TypeMapperFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("TypeMapperFunction")
    }
}

#[derive(Clone, Debug)]
pub struct TypeMapperCompositeOrMerged {
    pub mapper1: Box<TypeMapper>,
    pub mapper2: Box<TypeMapper>,
}

impl TypeMapper {
    pub fn new_simple(source: Rc<Type>, target: Rc<Type>) -> Self {
        Self::Simple(TypeMapperSimple { source, target })
    }

    pub fn new_array(sources: Vec<Rc<Type>>, targets: Option<Vec<Rc<Type>>>) -> Self {
        Self::Array(TypeMapperArray { sources, targets })
    }

    pub fn new_function(func: fn(&TypeChecker, &Type) -> Rc<Type>) -> Self {
        Self::Function(TypeMapperFunction { func })
    }

    pub fn new_composite(mapper1: TypeMapper, mapper2: TypeMapper) -> Self {
        Self::Composite(TypeMapperCompositeOrMerged {
            mapper1: Box::new(mapper1),
            mapper2: Box::new(mapper2),
        })
    }

    pub fn new_merged(mapper1: TypeMapper, mapper2: TypeMapper) -> Self {
        Self::Merged(TypeMapperCompositeOrMerged {
            mapper1: Box::new(mapper1),
            mapper2: Box::new(mapper2),
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Ternary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
}

impl TryFrom<i32> for Ternary {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            value if value == Ternary::False as i32 => Ok(Ternary::False),
            value if value == Ternary::Unknown as i32 => Ok(Ternary::Unknown),
            value if value == Ternary::Maybe as i32 => Ok(Ternary::Maybe),
            value if value == Ternary::True as i32 => Ok(Ternary::True),
            _ => Err(()),
        }
    }
}

impl BitAndAssign for Ternary {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = (*self as i32 & rhs as i32).try_into().unwrap();
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AssignmentDeclarationKind {
    None,
    ExportsProperty,
    ModuleExports,
    PrototypeProperty,
    ThisProperty,
    Property,
    Prototype,
    ObjectDefinePropertyValue,
    ObjectDefinePropertyExports,
    ObjectDefinePrototypeProperty,
}

#[derive(Clone, Debug)]
pub struct DiagnosticMessage {
    pub key: &'static str,
    pub category: DiagnosticCategory,
    pub code: u32,
    pub message: &'static str,
}

#[derive(Clone, Debug)]
pub struct DiagnosticMessageChain {
    pub message_text: String,
    pub code: u32,
    pub next: Option<Vec<DiagnosticMessageChain>>,
}

impl DiagnosticMessageChain {
    pub fn new(message_text: String, code: u32, next: Option<Vec<DiagnosticMessageChain>>) -> Self {
        Self {
            message_text,
            code,
            next,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Diagnostic {
    DiagnosticWithLocation(DiagnosticWithLocation),
    DiagnosticWithDetachedLocation(DiagnosticWithDetachedLocation),
}

impl Diagnostic {
    pub fn as_diagnostic_with_detached_location(&self) -> &DiagnosticWithDetachedLocation {
        enum_unwrapped!(self, [Diagnostic, DiagnosticWithDetachedLocation])
    }
}

pub trait DiagnosticInterface: DiagnosticRelatedInformationInterface {
    fn maybe_related_information(&self) -> Option<&[Rc<DiagnosticRelatedInformation>]>;
    fn set_related_information(
        &mut self,
        related_information: Vec<Rc<DiagnosticRelatedInformation>>,
    );
}

#[derive(Clone, Debug)]
pub struct BaseDiagnostic {
    _diagnostic_related_information: BaseDiagnosticRelatedInformation,
    related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
}

impl BaseDiagnostic {
    pub fn new(
        diagnostic_related_information: BaseDiagnosticRelatedInformation,
        related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
    ) -> Self {
        Self {
            _diagnostic_related_information: diagnostic_related_information,
            related_information,
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnostic {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on BaseDiagnostic")
    }

    fn code(&self) -> u32 {
        self._diagnostic_related_information.code()
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic_related_information.file()
    }

    fn start(&self) -> isize {
        self._diagnostic_related_information.start()
    }

    fn length(&self) -> isize {
        self._diagnostic_related_information.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic_related_information.message_text()
    }
}

impl DiagnosticInterface for BaseDiagnostic {
    fn maybe_related_information(&self) -> Option<&[Rc<DiagnosticRelatedInformation>]> {
        self.related_information.as_deref()
    }

    fn set_related_information(
        &mut self,
        related_information: Vec<Rc<DiagnosticRelatedInformation>>,
    ) {
        self.related_information = Some(related_information);
    }
}

impl DiagnosticRelatedInformationInterface for Diagnostic {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        Some(self)
    }

    fn code(&self) -> u32 {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.code()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.code()
            }
        }
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.file()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.file()
            }
        }
    }

    fn start(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.start()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.start()
            }
        }
    }

    fn length(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.length()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.length()
            }
        }
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.message_text()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.message_text()
            }
        }
    }
}

impl DiagnosticInterface for Diagnostic {
    fn maybe_related_information(&self) -> Option<&[Rc<DiagnosticRelatedInformation>]> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.maybe_related_information()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.maybe_related_information()
            }
        }
    }

    fn set_related_information(
        &mut self,
        related_information: Vec<Rc<DiagnosticRelatedInformation>>,
    ) {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.set_related_information(related_information)
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.set_related_information(related_information)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum DiagnosticMessageText {
    String(String),
    DiagnosticMessageChain(DiagnosticMessageChain),
}

impl From<String> for DiagnosticMessageText {
    fn from(string: String) -> Self {
        DiagnosticMessageText::String(string)
    }
}

impl From<DiagnosticMessageChain> for DiagnosticMessageText {
    fn from(diagnostic_message_chain: DiagnosticMessageChain) -> Self {
        DiagnosticMessageText::DiagnosticMessageChain(diagnostic_message_chain)
    }
}

pub trait DiagnosticRelatedInformationInterface {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic>;
    fn code(&self) -> u32;
    fn file(&self) -> Option<Rc<SourceFile>>;
    fn start(&self) -> isize;
    fn length(&self) -> isize;
    fn message_text(&self) -> &DiagnosticMessageText;
}

#[derive(Clone, Debug)]
pub enum DiagnosticRelatedInformation {
    BaseDiagnosticRelatedInformation(BaseDiagnosticRelatedInformation),
    Diagnostic(Diagnostic),
}

impl DiagnosticRelatedInformation {
    pub fn as_diagnostic_with_detached_location(&self) -> &DiagnosticWithDetachedLocation {
        enum_unwrapped!(
            self,
            [
                DiagnosticRelatedInformation,
                Diagnostic,
                DiagnosticWithDetachedLocation
            ]
        )
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticRelatedInformation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.maybe_as_diagnostic(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => {
                diagnostic.maybe_as_diagnostic()
            }
        }
    }

    fn code(&self) -> u32 {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.code(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.code(),
        }
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.file(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.file(),
        }
    }

    fn start(&self) -> isize {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.start(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.start(),
        }
    }

    fn length(&self) -> isize {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.length(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.length(),
        }
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.message_text(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.message_text(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BaseDiagnosticRelatedInformation {
    code: u32,
    file: Option<Weak<SourceFile>>,
    start: isize,
    length: isize,
    message_text: DiagnosticMessageText,
}

impl BaseDiagnosticRelatedInformation {
    pub fn new<TDiagnosticMessageText: Into<DiagnosticMessageText>>(
        code: u32,
        file: Option<Rc<SourceFile>>,
        start: isize,
        length: isize,
        message_text: TDiagnosticMessageText,
    ) -> Self {
        Self {
            code,
            file: file.map(|file| Rc::downgrade(&file)),
            start,
            length,
            message_text: message_text.into(),
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnosticRelatedInformation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        None
    }

    fn code(&self) -> u32 {
        self.code
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        self.file.as_ref().map(|weak| weak.upgrade().unwrap())
    }

    fn start(&self) -> isize {
        self.start
    }

    fn length(&self) -> isize {
        self.length
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        &self.message_text
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticWithLocation {
    _diagnostic: BaseDiagnostic,
}

impl DiagnosticWithLocation {
    pub fn new(base_diagnostic: BaseDiagnostic) -> Self {
        Self {
            _diagnostic: base_diagnostic,
        }
    }

    pub fn file_unwrapped(&self) -> Rc<SourceFile> {
        self.file().unwrap()
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithLocation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on DiagnosticWithLocation")
    }

    fn code(&self) -> u32 {
        self._diagnostic.code()
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithLocation {
    fn maybe_related_information(&self) -> Option<&[Rc<DiagnosticRelatedInformation>]> {
        self._diagnostic.maybe_related_information()
    }

    fn set_related_information(
        &mut self,
        related_information: Vec<Rc<DiagnosticRelatedInformation>>,
    ) {
        self._diagnostic
            .set_related_information(related_information)
    }
}

impl From<DiagnosticWithLocation> for Diagnostic {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Diagnostic::DiagnosticWithLocation(diagnostic_with_location)
    }
}

impl From<DiagnosticWithLocation> for DiagnosticRelatedInformation {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithLocation(
            diagnostic_with_location,
        ))
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticWithDetachedLocation {
    _diagnostic: BaseDiagnostic,
    pub file_name: String,
}

impl DiagnosticWithDetachedLocation {
    pub fn new(base_diagnostic: BaseDiagnostic, file_name: String) -> Self {
        Self {
            _diagnostic: base_diagnostic,
            file_name,
        }
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithDetachedLocation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on DiagnosticWithDetachedLocation")
    }

    fn code(&self) -> u32 {
        self._diagnostic.code()
    }

    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithDetachedLocation {
    fn maybe_related_information(&self) -> Option<&[Rc<DiagnosticRelatedInformation>]> {
        self._diagnostic.maybe_related_information()
    }

    fn set_related_information(
        &mut self,
        related_information: Vec<Rc<DiagnosticRelatedInformation>>,
    ) {
        self._diagnostic
            .set_related_information(related_information)
    }
}

impl From<DiagnosticWithDetachedLocation> for Diagnostic {
    fn from(diagnostic_with_detached_location: DiagnosticWithDetachedLocation) -> Self {
        Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModuleResolutionKind {
    Classic = 1,
    NodeJs = 2,
    Node12 = 3,
    NodeNext = 99,
}
