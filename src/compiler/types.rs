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

impl Node for BaseNode {
    fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

pub trait Node {
    fn kind(&self) -> SyntaxKind;
}

pub struct NodeArray<TItem: Node> {
    _nodes: Vec<TItem>,
}

pub trait Statement: Node {}

pub struct EmptyStatement {
    _node: BaseNode,
}

impl Node for EmptyStatement {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl Statement for EmptyStatement {}

pub struct SourceFile {}

pub trait Program {
    fn get_source_files(&self) -> &[SourceFile];
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic>;
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

pub struct Diagnostic {}

pub struct NodeFactory {}
