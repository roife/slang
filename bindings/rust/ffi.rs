#![allow(non_snake_case)]
#![allow(clippy::module_inception)]

mod cxx_sv;

use cxx::{SharedPtr, UniquePtr};

pub use cxx_sv::CxxSV;
pub use slang_ffi::*;

#[cxx::bridge]
mod slang_ffi {
    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/text/SourceLocation.h");

        type SourceLocation;

        fn offset(self: &SourceLocation) -> usize;

        type SourceRange;

        #[namespace = "wrapper"]
        fn source_range_start(range: &SourceRange) -> usize;

        #[namespace = "wrapper"]
        fn source_range_end(range: &SourceRange) -> usize;
    }

    impl UniquePtr<SourceLocation> {}

    impl UniquePtr<SourceRange> {}

    #[namespace = "slang"]
    unsafe extern "C++" {
        include!("slang/include/slang/numeric/SVInt.h");

        #[cxx_name = "logic_t"]
        type SVLogic;

        fn isUnknown(self: &SVLogic) -> bool;

        fn toChar(self: &SVLogic) -> c_char;

        #[namespace = "wrapper"]
        fn logic_t_value(logic: &SVLogic) -> u8;

        type SVInt;

        fn isSigned(self: &SVInt) -> bool;

        fn hasUnknown(self: &SVInt) -> bool;

        fn getBitWidth(self: &SVInt) -> u32;

        fn getRawPtr(self: &SVInt) -> *const u64;

        #[namespace = "wrapper"]
        fn SVInt_toString(svint: &SVInt) -> String;

        #[namespace = "wrapper"]
        fn SVInt_clone(svint: &SVInt) -> UniquePtr<SVInt>;

        #[namespace = "wrapper"]
        fn SVInt_eq(lhs: &SVInt, rhs: &SVInt) -> UniquePtr<SVLogic>;
    }

    impl UniquePtr<SVLogic> {}

    impl UniquePtr<SVInt> {}

    #[namespace = "slang::parsing"]
    unsafe extern "C++" {
        include!("slang/include/slang/parsing/Token.h");

        #[cxx_name = "Trivia"]
        type SyntaxTrivia;

        fn getRawText(self: &SyntaxTrivia) -> CxxSV;

        #[namespace = "wrapper::parsing"]
        fn SyntaxTrivia_kind(trivia: &SyntaxTrivia) -> u8;

        #[cxx_name = "Token"]
        type SyntaxToken;

        fn isMissing(self: &SyntaxToken) -> bool;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_range(tok: &SyntaxToken) -> UniquePtr<SourceRange>;

        fn valueText(self: &SyntaxToken) -> CxxSV; // excapedText

        fn rawText(self: &SyntaxToken) -> CxxSV; // rawText

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_kind(tok: &SyntaxToken) -> u16;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_intValue(tok: &SyntaxToken) -> UniquePtr<SVInt>;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_bitValue(tok: &SyntaxToken) -> UniquePtr<SVLogic>;

        fn realValue(self: &SyntaxToken) -> f64;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_base(tok: &SyntaxToken) -> u8;

        #[namespace = "wrapper::parsing"]
        fn SyntaxToken_unit(tok: &SyntaxToken) -> u8;
    }

    impl UniquePtr<SyntaxTrivia> {}

    impl UniquePtr<SyntaxToken> {}

    #[namespace = "slang::syntax"]
    unsafe extern "C++" {
        include!("slang/include/slang/syntax/SyntaxNode.h");

        type SyntaxNode;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_range(node: &SyntaxNode) -> UniquePtr<SourceRange>;

        fn childNode(self: &SyntaxNode, idx: usize) -> *const SyntaxNode;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_childToken(node: &SyntaxNode, idx: usize) -> *const SyntaxToken;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_parent(node: &SyntaxNode) -> *const SyntaxNode;

        fn getChildCount(self: &SyntaxNode) -> usize;

        #[namespace = "wrapper::syntax"]
        fn SyntaxNode_kind(node: &SyntaxNode) -> u16;
    }

    #[namespace = "slang::syntax"]
    unsafe extern "C++" {
        include!("slang/bindings/rust/ffi/wrapper.h");
        include!("slang/include/slang/syntax/SyntaxTree.h");

        type SyntaxTree;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_fromText(text: CxxSV, name: CxxSV, path: CxxSV) -> SharedPtr<SyntaxTree>;

        #[namespace = "wrapper::syntax"]
        fn SyntaxTree_root(tree: &SyntaxTree) -> *const SyntaxNode;
    }

    impl SharedPtr<SyntaxTree> {}

    // StringView
    unsafe extern "C++" {
        include!("slang/bindings/rust/ffi/string_view.h");

        #[namespace = "std"]
        #[cxx_name = "string_view"]
        type CxxSV<'a> = crate::CxxSV<'a>;
    }
}

macro_rules! forward_functions {
    (fn $name:ident(&self $(, $arg:ident: $arg_ty:ty)*) -> $ret:ty |> $ffi_fn:ident; $($tt:tt)*) => {
        #[inline]
        pub fn $name(&self $(, $arg: $arg_ty)*) -> $ret {
            slang_ffi::$ffi_fn(self $(, $arg)*)
        }
        forward_functions!($($tt)*);
    };
    (fn $name:ident($($arg:ident: $arg_ty:ty),*) -> $ret:ty |> $ffi_fn:ident; $($tt:tt)*) => {
        #[inline]
        pub fn $name($($arg: $arg_ty),*) -> $ret {
            slang_ffi::$ffi_fn($($arg),*)
        }
        forward_functions!($($tt)*);
    };
    () => {};
}

macro_rules! impl_functions {
    (impl $type:ty { $($tt:tt)* }) => {
        impl $type {forward_functions!($($tt)*); }
    };
}

impl_functions! {
    impl SourceRange {
        fn start(&self) -> usize |> source_range_start;
        fn end(&self) -> usize |> source_range_end;
    }
}

impl_functions! {
    impl SyntaxTree {
        fn fromText(text: CxxSV, name: CxxSV, path: CxxSV) -> SharedPtr<SyntaxTree> |> SyntaxTree_fromText;
        fn root(&self) -> *const SyntaxNode |> SyntaxTree_root;
    }
}

impl_functions! {
    impl SyntaxNode {
        fn range(&self) -> UniquePtr<SourceRange> |> SyntaxNode_range;
        fn kind(&self) -> u16 |> SyntaxNode_kind;
        fn childToken(&self, idx: usize) -> *const SyntaxToken |> SyntaxNode_childToken;
        fn parent(&self) -> *const SyntaxNode |> SyntaxNode_parent;
    }
}

impl_functions! {
    impl SyntaxToken {
        fn range(&self) -> UniquePtr<SourceRange> |> SyntaxToken_range;
    }
}

impl_functions! {
    impl SyntaxTrivia {
        fn kind(&self) -> u8 |> SyntaxTrivia_kind;
    }
}

impl_functions! {
    impl SyntaxToken {
        fn kind(&self) -> u16 |> SyntaxToken_kind;
        fn intValue(&self) -> UniquePtr<SVInt> |> SyntaxToken_intValue;
        fn bitValue(&self) -> UniquePtr<SVLogic> |> SyntaxToken_bitValue;
        fn base(&self) -> u8 |> SyntaxToken_base;
        fn unit(&self) -> u8 |> SyntaxToken_unit;
    }
}

impl_functions! {
    impl SVLogic {
        fn value(&self) -> u8 |> logic_t_value;
    }
}

impl_functions! {
    impl SVInt {
        fn clone(&self) -> UniquePtr<SVInt> |> SVInt_clone;
        fn toString(&self) -> String |> SVInt_toString;
        fn eq(&self, rhs: &SVInt) -> UniquePtr<SVLogic> |> SVInt_eq;
    }
}
