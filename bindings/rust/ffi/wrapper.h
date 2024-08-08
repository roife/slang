#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxNode.h"
#include "slang/numeric/SVInt.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/text/SourceLocation.h"
#include "rust/cxx.h"

namespace wrapper {
  using SyntaxTrivia = ::slang::parsing::Trivia;
  using SyntaxToken = ::slang::parsing::Token;
  using SVInt = ::slang::SVInt;
  using logic_t = ::slang::logic_t;
  using SourceRange = ::slang::SourceRange;
  using SyntaxTree = ::slang::syntax::SyntaxTree;
  using SyntaxNode = ::slang::syntax::SyntaxNode;

  // SourceRange
  inline static size_t source_range_start(const slang::SourceRange& range) {
    return range.start().offset();
  }

  inline static size_t source_range_end(const slang::SourceRange& range) {
    return range.end().offset();
  }

  inline static uint8_t logic_t_value(const slang::logic_t& logic) {
    return logic.value;
  }

  inline static rust::string SVInt_toString(const SVInt& svint) {
    auto str = svint.toString(slang::LiteralBase::Decimal, false);
    return rust::String(str);
  }

  inline static std::unique_ptr<slang::SVInt> SVInt_clone(const SVInt& svint) {
    return std::make_unique<SVInt>(svint);
  }

  inline static std::unique_ptr<slang::logic_t> SVInt_eq(const SVInt& lhs, const SVInt& rhs) {
    return std::make_unique<logic_t>(lhs == rhs);
  }

  namespace parsing {
    // Trivia
    inline static uint8_t SyntaxTrivia_kind(const SyntaxTrivia& trivia) {
      return static_cast<uint8_t>(trivia.kind);
    }

    // Token
    inline static uint16_t SyntaxToken_kind(const SyntaxToken& token) {
      return static_cast<uint16_t>(token.kind);
    }

    inline static std::unique_ptr<SourceRange> SyntaxToken_range(const SyntaxToken& token) {
      auto range = token.range();
      return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
    }

    inline static std::unique_ptr<SVInt> SyntaxToken_intValue(const SyntaxToken& token) {
      return std::make_unique<SVInt>(token.intValue());
    }

    inline static std::unique_ptr<logic_t> SyntaxToken_bitValue(const SyntaxToken& token) {
      return std::make_unique<logic_t>(token.bitValue());
    }

    inline static uint8_t SyntaxToken_base(const SyntaxToken& token) {
      return static_cast<uint8_t>(token.numericFlags().base());
    }

    inline static uint8_t SyntaxToken_unit(const SyntaxToken& token) {
      return static_cast<uint8_t>(token.numericFlags().unit());
    }
  }

  namespace syntax {
    inline static std::shared_ptr<SyntaxTree> SyntaxTree_fromText(std::string_view text,
                                                                  std::string_view name,
                                                                  std::string_view path) {
      return SyntaxTree::fromText(text, name, path);
    }

    inline static const SyntaxNode* SyntaxTree_root(const SyntaxTree& tree) {
      return &tree.root();
    }

    inline static std::unique_ptr<SourceRange> SyntaxNode_range(const SyntaxNode& node) {
      auto range = node.sourceRange();
      return range == SourceRange::NoLocation ? nullptr : std::make_unique<SourceRange>(range);
    }

    inline static const SyntaxToken* SyntaxNode_childToken(const SyntaxNode& node, size_t index) {
      // Since the function returns a const ptr, so we garentee the node won't be modified.
      return (const_cast<SyntaxNode&>(node)).childTokenPtr(index);
    }

    inline static const SyntaxNode* SyntaxNode_parent(const SyntaxNode& node) {
      return node.parent;
    }

    inline static uint16_t SyntaxNode_kind(const SyntaxNode& node) {
      return static_cast<uint16_t>(node.kind);
    }
  }
}
