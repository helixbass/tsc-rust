use gc::{Finalize, Trace};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LogLevel {
    Terse,
    Normal,
    RequestTime,
    Verbose,
}

impl LogLevel {
    pub fn all() -> impl Iterator<Item = Self> {
        [Self::Terse, Self::Normal, Self::RequestTime, Self::Verbose].into_iter()
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Terse => "terse",
            Self::Normal => "normal",
            Self::RequestTime => "requestTime",
            Self::Verbose => "verbose",
        }
    }
}

// export const emptyArray: SortedReadonlyArray<never> = createSortedArray<never>();

pub trait Logger: Trace + Finalize {
    fn close(&self);
    fn has_level(&self, level: LogLevel) -> bool;
    fn logging_enabled(&self) -> bool;
    fn perftrc(&self, s: &str);
    fn info(&self, s: &str);
    fn start_group(&self);
    fn end_group(&self);
    fn msg(&self, s: &str, type_: Option<Msg>);
    fn get_log_file_name(&self) -> Option<String>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Msg {
    Err,
    Info,
    Perf,
}
