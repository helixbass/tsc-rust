pub trait HostCancellationToken {
    fn is_cancellation_requested(&self) -> bool;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LanguageServiceMode {
    Semantic,
    PartialSemantic,
    Syntactic,
}

pub struct ApplyCodeActionCommandResult {
    pub success_message: String,
}
