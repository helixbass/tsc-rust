use regex::Regex;
use std::ptr;
use std::rc::Rc;

use crate::{compare_values, trim_string, Comparison, Debug_};

lazy_static! {
    static ref version_reg_exp: Regex =
        Regex::new(r"(?i)^(0|[1-9]\d*)(?:\.(0|[1-9]\d*)(?:\.(0|[1-9]\d*)(?:\-([a-z0-9-.]+))?(?:\+([a-z0-9-.]+))?)?)?$")
            .unwrap();
    static ref prerelease_reg_exp: Regex =
        Regex::new(r"(?i)^(?:0|[1-9]\d*|[a-z-][a-z0-9-]*)(?:\.(?:0|[1-9]\d*|[a-z-][a-z0-9-]*))*$")
            .unwrap();
    static ref build_reg_exp: Regex = Regex::new(r"(?i)^[a-z0-9-]+(?:\.[a-z0-9-]+)*$").unwrap();
}

#[derive(Clone)]
pub struct Version {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
    pub prerelease: Vec<String>,
    pub build: Vec<String>,
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

    pub fn compare_to(&self, other: Option<&Self>) -> Comparison {
        if matches!(
            other,
            Some(other) if ptr::eq(self, other)
        ) {
            return Comparison::EqualTo;
        }
        if other.is_none() {
            return Comparison::GreaterThan;
        }
        let other = other.unwrap();
        let mut ret = compare_values(Some(self.major), Some(other.major));
        if ret != Comparison::EqualTo {
            return ret;
        }
        ret = compare_values(Some(self.minor), Some(other.minor));
        if ret != Comparison::EqualTo {
            return ret;
        }
        ret = compare_values(Some(self.patch), Some(other.patch));
        if ret != Comparison::EqualTo {
            return ret;
        }
        compare_prerelease_identifiers(&self.prerelease, &other.prerelease)
    }

    pub fn increment(&self, field: &str /*"major" | "minor" | "patch"*/) -> Self {
        match field {
            "major" => Self::new(self.major + 1, Some(0), Some(0), None, None),
            "minor" => Self::new(self.major, Some(self.minor + 1), Some(0), None, None),
            "patch" => Self::new(
                self.major,
                Some(self.minor),
                Some(self.patch + 1),
                None,
                None,
            ),
            _ => Debug_.assert_never(field, None),
        }
    }
}

impl From<&str> for Version {
    fn from(value: &str) -> Self {
        let result = Debug_.check_defined(try_parse_components(value), Some("Invalid version"));
        let Components {
            major,
            minor,
            patch,
            prerelease,
            build,
        } = result;
        Self::new(
            major,
            Some(minor),
            Some(patch),
            Some(&prerelease),
            Some(&build),
        )
    }
}

thread_local! {
    static version_zero_: Rc<Version> = Rc::new(Version::new(0, Some(0), Some(0), None, None));
}

fn version_zero() -> Rc<Version> {
    version_zero_.with(|version_zero| version_zero.clone())
}

struct Components {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
    pub prerelease: String,
    pub build: String,
}

fn try_parse_components(text: &str) -> Option<Components> {
    let match_ = version_reg_exp.captures(text)?;

    let major = &match_[1];
    let minor = match_.get(2).map_or("0", |capture| capture.as_str());
    let patch = match_.get(3).map_or("0", |capture| capture.as_str());
    let prerelease = match_.get(4).map_or("", |capture| capture.as_str());
    let build = match_.get(5).map_or("", |capture| capture.as_str());
    if !prerelease.is_empty() && !prerelease_reg_exp.is_match(prerelease) {
        return None;
    }
    if !build.is_empty() && !build_reg_exp.is_match(build) {
        return None;
    }
    Some(Components {
        major: major.parse::<usize>().unwrap(),
        minor: minor.parse::<usize>().unwrap(),
        patch: patch.parse::<usize>().unwrap(),
        prerelease: prerelease.to_owned(),
        build: build.to_owned(),
    })
}

fn compare_prerelease_identifiers(_left: &[String], _right: &[String]) -> Comparison {
    unimplemented!()
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
        let version: VersionOrString = version.into();
        let version = match version {
            VersionOrString::String(version) => Rc::new((&*version).into()),
            VersionOrString::Version(version) => version,
        };
        test_disjunction(&version, &self._alternatives)
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

fn parse_hyphen(_left: &str, _right: &str, _comparators: &mut Vec<Comparator>) -> bool {
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

fn test_disjunction(version: &Version, alternatives: &[Vec<Comparator>]) -> bool {
    if alternatives.is_empty() {
        return true;
    }
    for alternative in alternatives {
        if test_alternative(version, alternative) {
            return true;
        }
    }
    false
}

fn test_alternative(version: &Version, comparators: &[Comparator]) -> bool {
    for comparator in comparators {
        if !test_comparator(version, comparator.operator, &comparator.operand) {
            return false;
        }
    }
    true
}

fn test_comparator(version: &Version, operator: ComparatorOperator, operand: &Version) -> bool {
    let cmp = version.compare_to(Some(operand));
    match operator {
        ComparatorOperator::LessThan => cmp == Comparison::LessThan,
        ComparatorOperator::LessThanOrEqual => cmp != Comparison::GreaterThan,
        ComparatorOperator::GreaterThan => cmp == Comparison::GreaterThan,
        ComparatorOperator::GreaterThanOrEqual => cmp != Comparison::LessThan,
        ComparatorOperator::Equal => cmp == Comparison::EqualTo,
    }
}
