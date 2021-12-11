pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

#[derive(Copy, Clone)]
pub enum SyntaxKind {
    EndOfFileToken,
    SemicolonToken,
    AsteriskToken,
    EmptyStatement,
}

pub struct BaseNode {
    pub kind: SyntaxKind,
}

pub trait NodeInterface {
    fn kind(&self) -> SyntaxKind;
}

pub enum Node {
    Statement(Statement),
}

impl NodeInterface for Node {
    fn kind(&self) -> SyntaxKind {
        match self {
            Node::Statement(statement) => statement.kind(),
        }
    }
}

pub struct NodeArray {
    _nodes: Vec<Node>,
}

pub enum Statement {
    EmptyStatement(EmptyStatement),
}

impl NodeInterface for Statement {
    fn kind(&self) -> SyntaxKind {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.kind(),
        }
    }
}

impl From<Statement> for Node {
    fn from(statement: Statement) -> Self {
        Node::Statement(statement)
    }
}

pub struct EmptyStatement {
    _node: BaseNode,
}

impl NodeInterface for EmptyStatement {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<EmptyStatement> for Statement {
    fn from(empty_statement: EmptyStatement) -> Self {
        Statement::EmptyStatement(empty_statement)
    }
}

pub struct SourceFile {}

pub trait Program {
    fn get_source_files(&self) -> &[SourceFile];
    fn get_semantic_diagnostics(&self) -> Vec<Box<dyn Diagnostic>>;
}

pub enum StructureIsReused {
    Not,
    Completely,
}

pub enum ExitStatus {
    Success,
    DiagnosticsPresent_OutputsGenerated,
}

pub struct ParsedCommandLine {
    pub file_names: Vec<String>,
}

pub struct CreateProgramOptions<'config> {
    pub root_names: &'config [String],
}

#[non_exhaustive]
pub struct CharacterCodes;
impl CharacterCodes {
    pub const asterisk: char = '*';
    pub const slash: char = '/';
}

pub trait ModuleResolutionHost {
    fn read_file(&self, file_name: &str) -> Option<String>;
}

pub trait CompilerHost: ModuleResolutionHost {
    fn get_source_file(&self, file_name: &str) -> Option<SourceFile>;
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub struct DiagnosticMessage {
    key: String,
    category: DiagnosticCategory,
    code: u32,
    message: String,
}

pub trait Diagnostic: DiagnosticRelatedInformation {}

pub trait DiagnosticRelatedInformation {}

pub struct DiagnosticWithDetachedLocation {
    pub file_name: String,
    pub start: usize,
    pub length: usize,
}

impl DiagnosticRelatedInformation for DiagnosticWithDetachedLocation {}

impl Diagnostic for DiagnosticWithDetachedLocation {}

pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

pub struct NodeFactory {}
