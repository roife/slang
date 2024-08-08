pub mod ast;
mod ffi;
mod syntax;

use cxx::{SharedPtr, UniquePtr};
pub use ffi::CxxSV;
use itertools::Either;
use std::{ffi::c_char, fmt, hash, iter, ops::Not, pin::Pin};
pub use syntax::{
    cursor::SyntaxCursor,
    iter::{SyntaxAncestors, SyntaxChildren, SyntaxIdxChildren, SyntaxPreorder},
    SyntaxKind, TokenKind, TriviaKind,
};

pub struct SVInt {
    _ptr: UniquePtr<ffi::SVInt>,
}

pub struct SVLogic {
    _ptr: UniquePtr<ffi::SVLogic>,
}

pub struct SourceLocation {
    _ptr: UniquePtr<ffi::SourceLocation>,
}

pub struct SourceRange {
    _ptr: UniquePtr<ffi::SourceRange>,
}

#[derive(Clone, Copy)]
pub struct SyntaxNode<'a> {
    _ptr: Pin<&'a ffi::SyntaxNode>,
}

#[derive(Clone, Copy)]
pub struct SyntaxToken<'a> {
    _ptr: Pin<&'a ffi::SyntaxToken>,
}

#[derive(Clone)]
pub struct SyntaxTree {
    _ptr: SharedPtr<ffi::SyntaxTree>,
}

pub struct SyntaxTrivia {
    _ptr: UniquePtr<ffi::SyntaxTrivia>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TimeUnit {
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
    Picoseconds,
    Femtoseconds,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralBase {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bit {
    L,
    H,
    X,
    Z,
}

impl SourceLocation {
    const NO_LOCATION: usize = (1usize << 36) - 1;

    #[inline]
    pub fn offset(&self) -> Option<usize> {
        let offset = self._ptr.offset();
        (offset == Self::NO_LOCATION).not().then_some(offset)
    }
}

impl fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceLocation")
            .field("offset", &self.offset())
            .finish()
    }
}

impl SourceRange {
    #[inline]
    fn from_unique_ptr(_ptr: UniquePtr<ffi::SourceRange>) -> Option<Self> {
        _ptr.is_null().not().then_some(SourceRange { _ptr })
    }

    #[inline]
    pub fn start(&self) -> usize {
        self._ptr.start()
    }

    #[inline]
    pub fn end(&self) -> usize {
        self._ptr.end()
    }
}

impl fmt::Debug for SourceRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceRange")
            .field("start", &self.start())
            .field("end", &self.end())
            .finish()
    }
}

impl SVLogic {
    #[inline]
    pub fn is_unknown(&self) -> bool {
        self._ptr.isUnknown()
    }

    #[inline]
    pub fn char(&self) -> c_char {
        self._ptr.toChar()
    }

    #[inline]
    pub fn bit(&self) -> Bit {
        const X: u8 = 1 << 7;
        const Z: u8 = 1 << 6;
        match self._ptr.value() {
            0 => Bit::L,
            1 => Bit::H,
            X => Bit::X,
            Z => Bit::Z,
            _ => unreachable!(),
        }
    }
}

impl SVInt {
    #[inline]
    pub fn is_signed(&self) -> bool {
        self._ptr.isSigned()
    }

    #[inline]
    pub fn has_unknown(&self) -> bool {
        self._ptr.hasUnknown()
    }

    #[inline]
    pub fn get_bit_width(&self) -> usize {
        self._ptr.getBitWidth() as usize
    }

    #[inline]
    pub fn is_single_word(&self) -> bool {
        const CHAR_BIT: usize = core::ffi::c_char::BITS as usize;
        const BITS_PER_WORD: usize = core::mem::size_of::<u64>() * CHAR_BIT;
        self.get_bit_width() <= BITS_PER_WORD && !self.has_unknown()
    }

    #[inline]
    pub fn get_single_word(&self) -> Option<u64> {
        self.is_single_word()
            .then(|| unsafe { *self._ptr.getRawPtr() })
    }

    #[inline]
    pub fn logic_eq(&self, other: &SVInt) -> SVLogic {
        let logic = self._ptr.eq(&other._ptr);
        SVLogic { _ptr: logic }
    }
}

impl fmt::Debug for SVInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SVInt")
            .field("to_string", &self.to_string())
            .finish()
    }
}

impl Clone for SVInt {
    fn clone(&self) -> Self {
        SVInt {
            _ptr: self._ptr.clone(),
        }
    }
}

impl PartialEq for SVInt {
    fn eq(&self, other: &Self) -> bool {
        let logic = self.logic_eq(other);
        logic.bit() == Bit::H
    }
}

impl Eq for SVInt {}

impl hash::Hash for SVInt {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self._ptr.getRawPtr().hash(state)
    }
}

impl ToString for SVInt {
    fn to_string(&self) -> String {
        self._ptr.toString()
    }
}

impl SyntaxTrivia {
    #[inline]
    pub fn get_raw_text(&self) -> CxxSV {
        self._ptr.getRawText()
    }

    #[inline]
    pub fn kind(&self) -> TriviaKind {
        TriviaKind::from_id(self._ptr.kind())
    }
}

impl SyntaxToken<'_> {
    #[inline]
    fn from_raw_ptr(_ptr: *const ffi::SyntaxToken) -> Option<Self> {
        _ptr.is_null().not().then_some(SyntaxToken {
            _ptr: unsafe { Pin::new_unchecked(&*_ptr) },
        })
    }

    #[inline]
    pub fn is_missing(&self) -> bool {
        self._ptr.isMissing()
    }

    #[inline]
    pub fn range(&self) -> Option<SourceRange> {
        SourceRange::from_unique_ptr(self._ptr.range())
    }

    #[inline]
    pub fn value_text(&self) -> CxxSV {
        self._ptr.valueText()
    }

    #[inline]
    pub fn raw_text(&self) -> CxxSV {
        self._ptr.rawText()
    }

    #[inline]
    pub fn kind(&self) -> TokenKind {
        TokenKind::from_id(self._ptr.kind())
    }

    #[inline]
    pub fn int(&self) -> Option<SVInt> {
        matches!(self.kind(), TokenKind::INTEGER_LITERAL).then(|| SVInt {
            _ptr: self._ptr.intValue(),
        })
    }

    #[inline]
    pub fn bits(&self) -> Option<SVLogic> {
        matches!(self.kind(), TokenKind::UNBASED_UNSIZED_LITERAL).then(|| SVLogic {
            _ptr: self._ptr.bitValue(),
        })
    }

    #[inline]
    pub fn real(&self) -> Option<f64> {
        matches!(
            self.kind(),
            TokenKind::REAL_LITERAL | TokenKind::TIME_LITERAL
        )
        .then(|| self._ptr.realValue())
    }

    #[inline]
    pub fn base(&self) -> Option<LiteralBase> {
        matches!(self.kind(), TokenKind::INTEGER_BASE)
            .then(|| unsafe { std::mem::transmute::<u8, LiteralBase>(self._ptr.base()) })
    }

    #[inline]
    pub fn time_unit(&self) -> Option<TimeUnit> {
        matches!(self.kind(), TokenKind::TIME_LITERAL)
            .then(|| unsafe { std::mem::transmute::<u8, TimeUnit>(self._ptr.unit()) })
    }
}

impl fmt::Debug for SyntaxToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxToken")
            .field("kind", &self.kind())
            .field("range", &self.range())
            .field("value_text", &self.value_text())
            .finish()
    }
}

impl<'a> SyntaxNode<'a> {
    #[inline]
    fn from_raw_ptr(_ptr: *const ffi::SyntaxNode) -> Option<Self> {
        _ptr.is_null().not().then_some(SyntaxNode {
            _ptr: unsafe { Pin::new_unchecked(&*_ptr) },
        })
    }

    #[inline]
    pub fn walk(&self) -> SyntaxCursor<'a> {
        SyntaxCursor::new(*self)
    }

    #[inline]
    pub fn range(&self) -> Option<SourceRange> {
        SourceRange::from_unique_ptr(self._ptr.range())
    }

    #[inline]
    pub fn child_node(&self, idx: usize) -> Option<SyntaxNode<'a>> {
        SyntaxNode::from_raw_ptr(self._ptr.childNode(idx))
    }

    // not-null
    #[inline]
    pub fn child_token(&self, idx: usize) -> Option<SyntaxToken<'a>> {
        SyntaxToken::from_raw_ptr(self._ptr.childToken(idx))
    }

    #[inline]
    pub fn child_count(&self) -> usize {
        self._ptr.getChildCount()
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind::from_id(self._ptr.kind())
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode<'a>> {
        SyntaxNode::from_raw_ptr(self._ptr.parent())
    }

    #[inline]
    pub fn child(&self, idx: usize) -> Option<SyntaxElement<'a>> {
        // TODO: we have to visit twice to get the child, this is not efficient
        if idx >= self.child_count() {
            None
        } else if let Some(node) = self.child_node(idx) {
            Some(SyntaxElement::Node(node))
        } else {
            self.child_token(idx)
                .map(|tok| SyntaxElement::Token { parent: *self, tok })
        }
    }

    #[inline]
    pub fn children_with_idx(&self) -> SyntaxIdxChildren<'a> {
        SyntaxIdxChildren::new(*self)
    }

    #[inline]
    pub fn children(&self) -> SyntaxChildren<'a> {
        SyntaxChildren::new(*self)
    }
}

impl fmt::Debug for SyntaxNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxNode")
            .field("kind", &self.kind())
            .field("range", &self.range())
            .field("child_count", &self.child_count())
            .finish()
    }
}

impl PartialEq for SyntaxNode<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Just compare pointer
        std::ptr::eq(
            Pin::as_ref(&self._ptr).get_ref(),
            Pin::as_ref(&other._ptr).get_ref(),
        )
    }
}

impl Eq for SyntaxNode<'_> {}

impl hash::Hash for SyntaxNode<'_> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        let ptr = Pin::as_ref(&self._ptr).get_ref() as *const ffi::SyntaxNode;
        ptr.hash(state)
    }
}

impl SyntaxTree {
    #[inline]
    pub fn from_text(text: &str, name: &str, path: &str) -> SyntaxTree {
        SyntaxTree {
            _ptr: ffi::SyntaxTree::fromText(CxxSV::new(text), CxxSV::new(name), CxxSV::new(path)),
        }
    }

    #[inline]
    pub fn root(&self) -> Option<SyntaxNode> {
        SyntaxNode::from_raw_ptr(self._ptr.root())
    }
}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("<SyntaxTree>").finish()
    }
}

impl PartialEq for SyntaxTree {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let a = self._ptr.as_ref().unwrap();
        let b = other._ptr.as_ref().unwrap();
        std::ptr::eq(std::ptr::from_ref(a), std::ptr::from_ref(b))
    }
}

impl Eq for SyntaxTree {}

#[derive(Clone, Copy)]
pub enum SyntaxElement<'a> {
    Node(SyntaxNode<'a>),
    Token {
        parent: SyntaxNode<'a>,
        tok: SyntaxToken<'a>,
    },
}

impl<'a> SyntaxElement<'a> {
    pub fn from_node(node: SyntaxNode) -> SyntaxElement {
        SyntaxElement::Node(node)
    }

    pub fn from_token<'b>(parent: SyntaxNode<'b>, tok: SyntaxToken<'b>) -> SyntaxElement<'b> {
        SyntaxElement::Token { parent, tok }
    }

    pub fn as_node(&self) -> Option<&SyntaxNode<'a>> {
        match self {
            SyntaxElement::Node(node) => Some(node),
            SyntaxElement::Token { .. } => None,
        }
    }

    pub fn as_token(&self) -> Option<&SyntaxToken<'a>> {
        match self {
            SyntaxElement::Token { tok, .. } => Some(tok),
            SyntaxElement::Node(_) => None,
        }
    }

    pub fn child_count(&self) -> usize {
        match self {
            SyntaxElement::Node(node) => node.child_count(),
            SyntaxElement::Token { .. } => 0,
        }
    }

    pub fn child(&self, idx: usize) -> Option<SyntaxElement<'a>> {
        match self {
            SyntaxElement::Node(node) => node.child(idx),
            SyntaxElement::Token { .. } => None,
        }
    }

    pub fn range(&self) -> Option<SourceRange> {
        match self {
            SyntaxElement::Node(node) => node.range(),
            SyntaxElement::Token { tok, .. } => tok.range(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode<'a>> {
        match self {
            SyntaxElement::Node(node) => node.parent(),
            SyntaxElement::Token { parent, .. } => Some(*parent),
        }
    }

    pub fn kind(&self) -> SyntaxElementKind {
        match self {
            SyntaxElement::Node(node) => SyntaxElementKind::Node(node.kind()),
            SyntaxElement::Token { tok, .. } => SyntaxElementKind::Token(tok.kind()),
        }
    }

    pub fn children_with_idx(
        &self,
    ) -> Either<SyntaxIdxChildren<'a>, iter::Empty<(usize, SyntaxElement<'a>)>> {
        match self {
            SyntaxElement::Node(node) => Either::Left(node.children_with_idx()),
            SyntaxElement::Token { .. } => Either::Right(iter::empty()),
        }
    }

    pub fn children(&self) -> Either<SyntaxChildren<'a>, iter::Empty<SyntaxElement<'a>>> {
        match self {
            SyntaxElement::Node(node) => Either::Left(node.children()),
            SyntaxElement::Token { .. } => Either::Right(iter::empty()),
        }
    }
}

impl<'a> From<SyntaxNode<'a>> for SyntaxElement<'a> {
    fn from(node: SyntaxNode<'a>) -> SyntaxElement<'a> {
        SyntaxElement::Node(node)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElementKind {
    Node(SyntaxKind),
    Token(TokenKind),
}

impl SyntaxElementKind {
    pub fn is_list(&self) -> bool {
        match self {
            SyntaxElementKind::Node(kind) => kind.is_list(),
            SyntaxElementKind::Token(_) => false,
        }
    }
}

impl From<SyntaxKind> for SyntaxElementKind {
    fn from(kind: SyntaxKind) -> SyntaxElementKind {
        SyntaxElementKind::Node(kind)
    }
}

impl From<TokenKind> for SyntaxElementKind {
    fn from(kind: TokenKind) -> SyntaxElementKind {
        SyntaxElementKind::Token(kind)
    }
}

#[cfg(test)]
mod tests;
