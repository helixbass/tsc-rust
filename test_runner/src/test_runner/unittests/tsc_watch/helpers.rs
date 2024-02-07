use std::borrow::Borrow;

use gc::Gc;
use typescript_rust::{
    format_string_from_args, get_locale_specific_message, id_arena::Id, to_path, BaseDiagnostic,
    BaseDiagnosticRelatedInformationBuilder, Diagnostic, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticMessageOrDiagnosticMessageChain, DiagnosticMessageText, DiagnosticWithLocation, Node,
    NonEmpty, Program, ScriptReferenceHost,
};

pub fn get_diagnostic_message_chain(
    message: &DiagnosticMessage,
    args: impl Into<Option<Vec<String>>>,
    next: impl Into<Option<Vec<DiagnosticMessageChain>>>,
) -> DiagnosticMessageChain {
    let args: Option<Vec<String>> = args.into();
    let next: Option<Vec<DiagnosticMessageChain>> = next.into();
    let mut text = get_locale_specific_message(message);
    if let Some(args) = args.non_empty() {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain::new(text, message.category, message.code, next)
}

pub fn get_diagnostic_of_file_from(
    file: Option<impl Borrow<Node /*SourceFile*/>>,
    start: Option<isize>,
    length: Option<isize>,
    message: impl Into<DiagnosticMessageOrDiagnosticMessageChain>,
    args: Option<Vec<String>>,
) -> Id<Diagnostic> {
    let message: DiagnosticMessageOrDiagnosticMessageChain = message.into();
    let file = file.node_wrappered();
    Gc::new({
        let base_diagnostic = BaseDiagnostic::new(
            BaseDiagnosticRelatedInformationBuilder::default()
                .file(file.clone())
                .start(start)
                .length(length)
                .category(message.category())
                .code(message.code())
                .message_text(match message {
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessageChain(message) => {
                        DiagnosticMessageText::from(message)
                    }
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessage(message) => {
                        get_diagnostic_message_chain(message, args, None)
                            .message_text
                            .into()
                    }
                })
                .build()
                .unwrap(),
            None,
        );
        if file.is_some() {
            DiagnosticWithLocation::new(base_diagnostic).into()
        } else {
            base_diagnostic.into()
        }
    })
}

pub fn get_diagnostic_of_file_from_program(
    program: &Program,
    file_path: &str,
    start: isize,
    length: isize,
    message: impl Into<DiagnosticMessageOrDiagnosticMessageChain>,
    args: Option<Vec<String>>,
) -> Id<Diagnostic> {
    get_diagnostic_of_file_from(
        program.get_source_file_by_path(&to_path(
            file_path,
            Some(&program.get_current_directory()),
            |s: &str| s.to_lowercase(),
        )),
        Some(start),
        Some(length),
        message,
        args,
    )
}
