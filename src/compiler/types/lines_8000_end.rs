#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::BitAndAssign;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use super::{Diagnostic, SourceFile, SymbolWriter};
use crate::{NodeBuilder, Number, SortedArray, WeakSelf};
use local_macros::{ast_type, enum_unwrapped, symbol_type, type_type};

pub struct Printer {
    pub current_source_file: Option<Rc<SourceFile>>,
    pub writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    pub write: fn(&Printer, &str),
}

pub struct PrinterOptions {}

pub trait EmitTextWriter: SymbolWriter {
    fn write(&mut self, s: &str);
    fn write_trailing_semicolon(&mut self, text: &str);
    fn get_text(&self) -> String;
}

pub trait ModuleSpecifierResolutionHost {}

pub trait SymbolTracker {}

pub struct TextSpan {
    pub start: isize,
    pub length: isize,
}

#[derive(Debug)]
pub struct DiagnosticCollection {
    pub file_diagnostics: HashMap<String, SortedArray<Rc<Diagnostic>>>,
}

bitflags! {
    pub struct ListFormat: u32 {
        const None = 0;

        const SingleLine = 0;

        const BarDelimited = 1 << 2;
        const AmpersandDelimited = 1 << 3;
        const CommaDelimited = 1 << 4;
        const AsteriskDelimited = 1 << 5;
        const DelimitersMask = Self::BarDelimited.bits | Self::AmpersandDelimited.bits | Self::CommaDelimited.bits | Self::AsteriskDelimited.bits;

        const SpaceBetweenBraces = 1 << 8;
        const SpaceBetweenSiblings = 1 << 9;

        const NoSpaceIfEmpty = 1 << 19;
        const SpaceAfterList = 1 << 21;

        const SingleLineTypeLiteralMembers = Self::SingleLine.bits | Self::SpaceBetweenBraces.bits | Self::SpaceBetweenSiblings.bits;

        const UnionTypeConstituents = Self::BarDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
    }
}

#[derive(Clone, Debug)]
pub struct PseudoBigInt {
    pub negative: bool,
    pub base_10_value: String,
}

impl PseudoBigInt {
    pub fn new(negative: bool, base_10_value: String) -> Self {
        Self {
            negative,
            base_10_value,
        }
    }
}
