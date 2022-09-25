use regex::Regex;
use std::rc::Rc;

use crate::{trim_string, Debug_};

#[derive(Clone)]
pub struct Version {
    pub version: &'static str,
}

impl Version {
    pub fn new(version: &'static str) -> Self {
        Self { version }
    }
}

pub struct VersionRange {
    _alternatives: Vec<Vec<Comparator>>,
}

impl VersionRange {
    pub fn new(spec: &str) -> Self {
        Self {
            _alternatives: if !spec.is_empty() {
                Debug_.check_defined(parse_range(spec), Some("Invalid range spec."))
            } else {
                vec![]
            },
        }
    }

    pub fn try_parse(text: &str) -> Option<Self> {
        let sets = parse_range(text);
        if let Some(sets) = sets {
            let mut range = Self::new("");
            range._alternatives = sets;
            return Some(range);
        }
        None
    }

    pub fn test<TVersion: Into<VersionOrString>>(&self, version: TVersion) -> bool {
        unimplemented!()
    }
}

pub enum VersionOrString {
    Version(Rc<Version>),
    String(String),
}

impl From<Rc<Version>> for VersionOrString {
    fn from(value: Rc<Version>) -> Self {
        Self::Version(value)
    }
}

impl From<String> for VersionOrString {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

struct Comparator {
    pub operator: ComparatorOperator,
    pub operand: Rc<Version>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ComparatorOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
}

lazy_static! {
    static ref logical_or_reg_exp: Regex = Regex::new(r"\|\|").unwrap();
    static ref whitespace_reg_exp: Regex = Regex::new(r"\s+").unwrap();
    static ref hyphen_reg_exp: Regex =
        Regex::new(r"(?i)^\s*([a-z0-9-+.*]+)\s+-\s+([a-z0-9-+.*]+)\s*$").unwrap();
    static ref range_reg_exp: Regex =
        Regex::new(r"(?i)^(~|\^|<|<=|>|>=|=)?\s*([a-z0-9-+.*]+)$").unwrap();
}

fn parse_range(text: &str) -> Option<Vec<Vec<Comparator>>> {
    let mut alternatives: Vec<Vec<Comparator>> = vec![];
    for mut range in logical_or_reg_exp.split(trim_string(text)) {
        if range.is_empty() {
            continue;
        }
        let mut comparators: Vec<Comparator> = vec![];
        range = trim_string(range);
        let match_ = hyphen_reg_exp.captures(range);
        if let Some(match_) = match_ {
            if !parse_hyphen(&match_[1], &match_[2], &mut comparators) {
                return None;
            }
        } else {
            for simple in whitespace_reg_exp.split(range) {
                let match_ = range_reg_exp.captures(trim_string(simple))?;
                if !parse_comparator(&match_[1], &match_[2], &mut comparators) {
                    return None;
                }
            }
        }
        alternatives.push(comparators);
    }
    Some(alternatives)
}

fn parse_hyphen(left: &str, right: &str, comparators: &mut Vec<Comparator>) -> bool {
    unimplemented!()
}

fn parse_comparator(operator: &str, text: &str, comparators: &mut Vec<Comparator>) -> bool {
    unimplemented!()
}
