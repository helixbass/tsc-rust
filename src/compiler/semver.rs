use regex::Regex;
use std::rc::Rc;

use crate::{trim_string, Debug_};

lazy_static! {
    static ref prerelease_reg_exp: Regex =
        Regex::new(r"(?i)^(?:0|[1-9]\d*|[a-z-][a-z0-9-]*)(?:\.(?:0|[1-9]\d*|[a-z-][a-z0-9-]*))*$")
            .unwrap();
    static ref build_reg_exp: Regex = Regex::new(r"(?i)^[a-z0-9-]+(?:\.[a-z0-9-]+)*$").unwrap();
}

#[derive(Clone)]
pub struct Version {
    major: usize,
    minor: usize,
    patch: usize,
    prerelease: Vec<String>,
    build: Vec<String>,
}

impl Version {
    pub fn new(
        major: usize,
        minor: Option<usize>,
        patch: Option<usize>,
        prerelease: Option<&str>,
        build: Option<&str>,
    ) -> Self {
        let minor = minor.unwrap_or(0);
        let patch = patch.unwrap_or(0);
        let prerelease = prerelease.unwrap_or("");
        let build = build.unwrap_or("");

        // Debug.assert(major >= 0, "Invalid argument: major");
        // Debug.assert(minor >= 0, "Invalid argument: minor");
        // Debug.assert(patch >= 0, "Invalid argument: patch");
        Debug_.assert(
            prerelease.is_empty() || prerelease_reg_exp.is_match(prerelease),
            Some("Invalid argument: prerelease"),
        );
        Debug_.assert(
            build.is_empty() || build_reg_exp.is_match(build),
            Some("Invalid argument: build"),
        );
        Self {
            major,
            minor,
            patch,
            prerelease: if !prerelease.is_empty() {
                prerelease.split(".").map(ToOwned::to_owned).collect()
            } else {
                vec![]
            },
            build: if !build.is_empty() {
                build.split(".").map(ToOwned::to_owned).collect()
            } else {
                vec![]
            },
        }
    }

    pub fn increment(&self, field: &str /*"major" | "minor" | "patch"*/) -> Self {
        unimplemented!()
    }
}

impl From<&str> for Version {
    fn from(value: &str) -> Self {
        unimplemented!()
    }
}

thread_local! {
    static version_zero_: Rc<Version> = Rc::new(Version::new(0, Some(0), Some(0), None, None));
}

fn version_zero() -> Rc<Version> {
    version_zero_.with(|version_zero| version_zero.clone())
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

// TODO: should use TryFrom for this?
impl From<&str> for ComparatorOperator {
    fn from(value: &str) -> Self {
        match value {
            "<" => Self::LessThan,
            "<=" => Self::LessThanOrEqual,
            ">" => Self::GreaterThan,
            ">=" => Self::GreaterThanOrEqual,
            "=" => Self::Equal,
            _ => panic!("Unexpected operator"),
        }
    }
}

lazy_static! {
    static ref logical_or_reg_exp: Regex = Regex::new(r"\|\|").unwrap();
    static ref whitespace_reg_exp: Regex = Regex::new(r"\s+").unwrap();
    static ref partial_reg_exp: Regex = Regex::new(r"(?i)^([xX*0]|[1-9]\d*)(?:\.([xX*0]|[1-9]\d*)(?:\.([xX*0]|[1-9]\d*)(?:-([a-z0-9-.]+))?(?:\+([a-z0-9-.]+))?)?)?$").unwrap();
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

struct Partial {
    pub version: Rc<Version>,
    pub major: String,
    pub minor: String,
    pub patch: String,
}

fn parse_partial(text: &str) -> Option<Partial> {
    let match_ = partial_reg_exp.captures(text)?;
    let major = &match_[1];
    let minor = match_.get(2).map_or("*", |capture| capture.as_str());
    let patch = match_.get(3).map_or("*", |capture| capture.as_str());
    let prerelease = match_.get(4).map(|capture| capture.as_str());
    let build = match_.get(5).map(|capture| capture.as_str());
    let version = Rc::new(Version::new(
        if is_wildcard(major) {
            0
        } else {
            major.parse::<usize>().unwrap()
        },
        Some(if is_wildcard(major) || is_wildcard(minor) {
            0
        } else {
            minor.parse::<usize>().unwrap()
        }),
        Some(
            if is_wildcard(major) || is_wildcard(minor) || is_wildcard(patch) {
                0
            } else {
                patch.parse::<usize>().unwrap()
            },
        ),
        prerelease,
        build,
    ));

    Some(Partial {
        version,
        major: major.to_owned(),
        minor: minor.to_owned(),
        patch: patch.to_owned(),
    })
}

fn parse_hyphen(left: &str, right: &str, comparators: &mut Vec<Comparator>) -> bool {
    unimplemented!()
}

fn parse_comparator(operator: &str, text: &str, comparators: &mut Vec<Comparator>) -> bool {
    let result = parse_partial(text);
    if result.is_none() {
        return false;
    }
    let result = result.unwrap();

    let Partial {
        version,
        major,
        minor,
        patch,
    } = result;
    if !is_wildcard(&major) {
        match operator {
            "~" => {
                comparators.push(create_comparator(">=", version.clone()));
                comparators.push(
                    create_comparator(
                        "<",
                        Rc::new(
                            version.increment(
                                if is_wildcard(&minor) {
                                    "major"
                                } else {
                                    "minor"
                                }
                            )
                        )
                    )
                );
            }
            "^" => {
                comparators.push(create_comparator(">=", version.clone()));
                comparators.push(create_comparator("<", Rc::new(version.increment(
                    if version.major > 0 || is_wildcard(&minor) {
                        "major"
                    } else if version.minor > 0 || is_wildcard(&patch) {
                        "minor"
                    } else {
                        "patch"
                    }
                ))));
            }
            "<" | ">=" => {
                comparators.push(create_comparator(operator, version.clone()));
            }
            "<=" | ">" => {
                comparators.push(
                    if is_wildcard(&minor) {
                        create_comparator(
                            if operator == "<=" {
                                "<"
                            } else {
                                ">="
                            },
                            Rc::new(version.increment("major"))
                        )
                    } else if is_wildcard(&patch) {
                        create_comparator(
                            if operator == "<=" {
                                "<"
                            } else {
                                ">="
                            },
                            Rc::new(version.increment("minor"))
                        )
                    } else {
                        create_comparator(operator, version.clone())
                    }
                );
            }
            "=" /*case undefined:*/ => {
                if is_wildcard(&minor) || is_wildcard(&patch) {
                    comparators.push(
                        create_comparator(
                            ">=",
                            version.clone()
                        )
                    );
                    comparators.push(
                        create_comparator(
                            "<",
                            Rc::new(version.increment(
                                if is_wildcard(&minor) {
                                    "major"
                                } else {
                                    "minor"
                                }
                            ))
                        )
                    );
                } else {
                    comparators.push(
                        create_comparator(
                            "=",
                            version.clone()
                        )
                    );
                }
            }
            _ => {
                return false;
            }
        }
    } else if operator == "<" || operator == ">" {
        comparators.push(create_comparator("<", version_zero()));
    }

    true
}

fn is_wildcard(part: &str) -> bool {
    matches!(part, "*" | "x" | "X")
}

fn create_comparator<TOperator: Into<ComparatorOperator>>(
    operator: TOperator,
    operand: Rc<Version>,
) -> Comparator {
    Comparator {
        operator: operator.into(),
        operand,
    }
}
