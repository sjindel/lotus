// -*- C++ -*-

#pragma once

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif

#ifndef __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "lptraits.h"
#include "lprc.h"

#include <cassert>
#include <functional>
#include <map>
#include <memory>
#include <stdint.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lpp {

typedef uint16_t nest_t;      ///< Maximum nesting level is 2^16.
typedef uint16_t col_t;       ///< Maximum column level is 2^16.
typedef int16_t nest_diff_t;  ///< Change in column is in [-2^15, 2^15).
typedef int16_t col_diff_t;   ///< Change in nesting is in [-2^15, 2^15).

template <typename cost_t, typename str_t>
class Doc;

template <typename cost_t, typename str_t>
class DocMan;

/// \brief Current state of the printer.
struct PrintState {
  PrintState(col_t width, nest_t nesting, col_t column)
      : width(width), nesting(nesting), column(column) {}
  const col_t width;     ///< Current line width
  const nest_t nesting;  ///< Current indentation level
  const col_t column;    ///< Current column
};

// TODO(sjindel): Convert this to latex with a grammar package.
/// \brief A high-level representation of an unformatted document.
///
/// 'Doc' represents a piece of document to be formatted. It has the following
/// inductive structure:
///
/// \verbatim
/// Doc ::= Fail                 -- an impossible document
///      |  Empty                -- an empty document
///      |  Cost(cost_t)         -- has a cost to render it
///      |  Alt(Doc, Doc)        -- flattening alternative
///      |  Text(width, str_t)   -- a piece of text with a logical width
///      |  Line                 -- line break
///      |  Doc * Doc            -- concatenation
///      |  Indent(col_diff_t)   -- add indentation (may be negative)
///      |  Column(col_diff_t)   -- add to the column (may be negative)
///      |  Doc + Doc            -- choice point
/// \endverbatim
///
/// See lptraits.h for requirements on 'cost_t' and 'str_t'.
template <typename cost_t, typename str_t>
class Doc {
 public:
  enum DocKind {
    DK_FAIL,
    DK_EMPTY,
    DK_TEXT,
    DK_COST,
    DK_LINE,
    DK_ALT,
    DK_CHOICE,
    DK_CAT,
    DK_INDENT,
    DK_COLUMN,
    DK_STATE
  };

  typedef std::function<const Doc *(PrintState)> state_fn_t;

  DocKind kind() const { return kind_; }

  /// The following are for internal use only!
  // TODO(sjindel): Devirtualize these.
  virtual bool shallowEq(const Doc &) const = 0;
  virtual size_t hash() const = 0;

  virtual ~Doc() {}

 protected:
  explicit Doc(DocKind kind) : kind_(kind) {}

 private:
  Doc(const Doc &) = delete;
  Doc &operator=(const Doc &) = delete;

  const DocKind kind_;
};

template <typename cost_t, typename str_t>
class Fail final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Fail() : super(super::DK_FAIL) {}

  bool shallowEq(const super &other) const override {
    return other.kind() == super::DK_FAIL;
  }

  size_t hash() const override { return 0; }
};

template <typename cost_t, typename str_t>
class Empty final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Empty() : Doc<cost_t, str_t>(super::DK_EMPTY) {}

  bool shallowEq(const super &other) const override {
    return other.kind() == super::DK_EMPTY;
  }

  size_t hash() const override { return 1; }
};

template <typename cost_t, typename str_t>
class Line final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Line() : super(super::DK_LINE) {}

  bool shallowEq(const super &other) const override {
    return other.kind() == super::DK_LINE;
  }

  size_t hash() const override { return 2; }
};

template <typename cost_t, typename str_t>
class Cost final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Cost(cost_t cost) : super(super::DK_COST), cost_(cost) {}

  cost_t cost() const { return cost_; }

  bool shallowEq(const super &other) const override {
    if (const Cost *c = dynamic_cast<const Cost *>(&other))
      return c->cost() == cost();
    return false;
  }

  size_t hash() const override {
    std::hash<cost_t> hash_fn;
    return hash_fn(cost());
  }

 private:
  const cost_t cost_;
};

template <typename cost_t, typename str_t>
class Indent final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  explicit Indent(col_diff_t indent)
      : super(super::DK_INDENT), indent_(indent) {}

  col_diff_t indent() const { return indent_; }

  bool shallowEq(const super &other) const override {
    if (const Indent *i = dynamic_cast<const Indent *>(&other))
      return i->indent() == indent();
    return false;
  }

  size_t hash() const override { return indent(); }

 private:
  const col_diff_t indent_;
};

template <typename cost_t, typename str_t>
class Column final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  explicit Column(col_diff_t column)
      : super(super::DK_COLUMN), column_(column) {}

  col_diff_t column() const { return column_; }

  bool shallowEq(const super &other) const override {
    if (const Column *i = dynamic_cast<const Column *>(&other))
      return i->column() == column();
    return false;
  }

  size_t hash() const override { return column(); }

 private:
  const col_diff_t column_;
};

template <typename cost_t, typename str_t>
class Text final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Text(col_t size, const str_t &text)
      : super(super::DK_TEXT), size_(size), text_(text) {}

  const str_t &text() const { return text_; }
  col_t size() const { return size_; }

  bool shallowEq(const super &other) const override {
    if (const Text *t = dynamic_cast<const Text *>(&other))
      return (t->text() == text()) && (t->size() == size());
    return false;
  }

  size_t hash() const override {
    std::hash<size_t> h1;
    std::hash<str_t> h2;
    return 65539 * h1(size()) + h2(text());
  }

 private:
  /// Size of the text to use when pretty-printing the document. It may not
  /// correspond to the actual width of the text; for example, we can add
  /// "fillers" by making 'Text' objects with a large size but an empty string.
  /// This way, the pretty-printing algorithm will make sure there is enough
  /// space to put a large document, but then will only actually print the empty
  /// string.
  const col_t size_;
  const str_t text_;
};

template <typename cost_t, typename str_t>
class Pair : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  const super *left() const { return left_; }
  const super *right() const { return right_; }

  bool shallowEq(const super &other) const override {
    if (const Pair *p = dynamic_cast<const Pair *>(&other)) {
      return (p->kind() == this->kind()) && (p->left() == left()) &&
             (p->right() == right());
    }
    return false;
  }

  size_t hash() const override {
    std::hash<const super *> h;
    return this->kind() * (h(left()) + h(right()));
  }

 protected:
  Pair(typename super::DocKind con, const super *left, const super *right)
      : super(con), left_(left), right_(right) {}

 private:
  const super *const left_;   ///< Borrowed from the 'DocMan' that owns 'this'.
  const super *const right_;  ///< Borrowed from the 'DocMan' that owns 'this'.
};

template <typename cost_t, typename str_t>
class Alt final : public Pair<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Alt(const super *left, const super *right)
      : Pair<cost_t, str_t>(super::DK_ALT, left, right) {}

 private:
  Alt(const Alt &) = delete;
  Alt &operator=(const Alt &) = delete;
};

template <typename cost_t, typename str_t>
class Choice final : public Pair<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Choice(const super *left, const super *right)
      : Pair<cost_t, str_t>(super::DK_CHOICE, left, right) {}
};

template <typename cost_t, typename str_t>
class Cat final : public Pair<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  Cat(const super *left, const super *right)
      : Pair<cost_t, str_t>(super::DK_CAT, left, right) {}
};

template <typename cost_t, typename str_t>
class State final : public Doc<cost_t, str_t> {
 public:
  typedef Doc<cost_t, str_t> super;

  State(typename super::state_fn_t call)
      : super(super::DK_STATE), call_(call) {}

  typename super::state_fn_t call() const { return call_; }

  bool shallowEq(const super &other) const override { return false; }

  size_t hash() const override {
    std::hash<const State<cost_t, str_t> *> h;
    return h(this);
  }

 private:
  const typename super::state_fn_t call_;
};

/// This class is for internal use only.
template <typename cost_t, typename str_t>
struct doc_hash_t {
  typedef const lpp::Doc<cost_t, str_t> *argument_type;
  typedef size_t value_type;

  value_type operator()(argument_type doc) const { return doc->hash(); }
};

template <typename cost_t, typename str_t>
struct doc_eq_t {
  typedef const lpp::Doc<cost_t, str_t> *argument_type;
  typedef bool value_type;

  value_type operator()(argument_type x, argument_type y) const {
    return x->shallowEq(*y);
  }
};

/// \brief File-local cache for 'Doc's.
///
/// This class holds a hash-consing table of 'Doc's. Thus any two documents
/// issued from the same 'DocMan' are structurally equal if and only if their
/// pointer addresses are equal. All documents should be constructed via the
/// factory functions in this class. Since the pointers returned from the class
/// are owned by the 'DocMan' object that issues them, they should not be used
/// outside the lifetime of the issuing object.
///
/// While is is possible to construct a 'Doc' directly without use of this
/// class, the hash-consing logic is necessary for the rendering algorithm to
/// function properly. It is intended that this class be used to cache the
/// 'Doc's generated for the AST of a single file, and that the 'DocMan' object
/// be destroyed after the rendering for that file is complete.
///
/// This class is not declared final to allow users of the class to inherit it
/// and thus gain unqualified access to its factory methods. A class that
/// inherits it might be responsible for, say, rendering of an individual file's
/// AST. Provided the issued documents are not used outside the subclass, this
/// method of use also provides a clean way to prevent the issued documents from
/// being used beyond the lifetime of their issuer.
template <typename cost_t, typename str_t>
class DocMan {
 public:
  DocMan();

  typedef const Doc<cost_t, str_t> doc_t;

  /// @{
  /// The variants of 'Cat' concatenates all their arguments, convering
  /// those which are strings to documents.

  /// \brief The unit of 'Cat' -- the empty document.
  doc_t *MakeCat();

  doc_t *MakeCat(doc_t *);

  doc_t *MakeCat(doc_t *, doc_t *);

  doc_t *MakeCat(const str_t &);

  template <typename A, typename B, typename... Cs>
  doc_t *MakeCat(const A &a, const B &b, Cs... cs);

  /// @}

  /// \brief Creates a document for text with a specified length.
  ///
  /// Use only if you need to provide a logical width of the string different
  /// from the one computed by the string traits instance.
  doc_t *MakeText(const str_t &str, size_t length);

  /// @{
  /// The variants of 'MakeChoice' chose the lease expensive of their arguments,
  /// convering those which are strings to documents.

  /// \brief The unit of 'MakeChoice' -- the failure document.
  doc_t *MakeChoice();

  doc_t *MakeChoice(doc_t *);

  doc_t *MakeChoice(doc_t *, doc_t *);

  template <typename A, typename B, typename... Cs>
  doc_t *MakeChoice(const A &a, const B &b, Cs... cs);

  /// @}

  /// \brief Make a document with cost \p cost that is rendered as 'Cat()'.
  doc_t *MakeCost(cost_t);

  /// \brief A document that is always rendered as a line break.
  ///
  /// It can not be flattened:
  /// \code{.cc} Flatten(MakeLine()) = MakeChoice() \endcode.
  doc_t *MakeLine();

  /// \brief A linebreak that has a cost to be rendered.
  doc_t *MakeLine(cost_t cost);

  /// \brief Add \p i to the current indentation (may be negative).
  doc_t *MakeIndent(col_diff_t i);

  /// \brief Add \p c to the current column (may be negative).
  doc_t *MakeColumn(col_diff_t c);

  /// \brief A document that can depend on the current print state.
  doc_t *MakeState(typename doc_t::state_fn_t);

  /// \brief A document that changes under flattening.
  ///
  /// See 'Flatten' for details.
  doc_t *MakeAlt(doc_t *normal, doc_t *flattened);

  virtual ~DocMan() {
    for (const auto *d : costs_) delete d;
    for (const auto *d : texts_) delete d;
    for (const auto *d : indents_) delete d;
    for (const auto *d : columns_) delete d;
    for (const auto *d : pairs_) delete d;
    for (const auto *d : states_) delete d;
  }

 private:
  DocMan(const DocMan &) = delete;
  DocMan &operator=(const DocMan &) = delete;

  const Fail<cost_t, str_t> fail_;
  const Empty<cost_t, str_t> empty_;
  const Line<cost_t, str_t> line_;

  typedef doc_hash_t<cost_t, str_t> hash_t;
  typedef doc_eq_t<cost_t, str_t> eq_t;

  // The hash-consing table is stratified to simplify the hash functions.
  std::unordered_set<doc_t *, hash_t, eq_t> costs_;
  std::unordered_set<doc_t *, hash_t, eq_t> texts_;
  std::unordered_set<doc_t *, hash_t, eq_t> indents_;
  std::unordered_set<doc_t *, hash_t, eq_t> columns_;
  std::unordered_set<doc_t *, hash_t, eq_t> pairs_;
  std::vector<doc_t *> states_;
};

//===----------------------------------------------------------------------===//

template <typename cost_t, typename str_t>
DocMan<cost_t, str_t>::DocMan()
    : fail_(), empty_(), line_() {}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCat() {
  return &empty_;
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCat(doc_t *doc) {
  return doc;
}

template <typename cost_t, typename str_t, typename Container>
const Doc<cost_t, str_t> *insertOrDelete(  // nofmt
    Container &C, const Doc<cost_t, str_t> *D) {
  auto x = C.insert(D);
  if (!x.second) delete D;
  return *x.first;
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCat(const str_t &str) {
  auto length = string_traits<str_t>::Length(str);
  return insertOrDelete(texts_, new Text<cost_t, str_t>(length, str));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCat(  // nofmt
    doc_t *left, doc_t *right) {
  return insertOrDelete(pairs_, new Cat<cost_t, str_t>(left, right));
}

template <typename cost_t, typename str_t>
template <typename A, typename B, typename... Cs>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCat(  // nofmt
    const A &a, const B &b, Cs... cs) {
  return MakeCat(MakeCat(a), MakeCat(b, cs...));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeText(  // nofmt
    const str_t &str, size_t length) {
  return insertOrDelete(texts_, new Text<cost_t, str_t>(length, str));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeChoice() {
  return &fail_;
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeChoice(doc_t *x) {
  return x;
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeChoice(  // nofmt
    doc_t *left, doc_t *right) {
  return insertOrDelete(pairs_, new Choice<cost_t, str_t>(left, right));
}

template <typename cost_t, typename str_t>
template <typename A, typename B, typename... Cs>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeChoice(const A &a,
                                                            const B &b,
                                                            Cs... cs) {
  return MakeChoice(MakeCat(a), MakeChoice(b, cs...));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeLine() {
  return &line_;
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeLine(cost_t cost) {
  return MakeCat(MakeCost(cost), MakeLine());
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeCost(cost_t cost) {
  return insertOrDelete(costs_, new Cost<cost_t, str_t>(cost));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeIndent(col_diff_t indent) {
  return insertOrDelete(indents_, new Indent<cost_t, str_t>(indent));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeColumn(col_diff_t column) {
  return insertOrDelete(columns_, new Column<cost_t, str_t>(column));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeAlt(  // nofmt
    doc_t *left, doc_t *right) {
  return insertOrDelete(pairs_, new Alt<cost_t, str_t>(left, right));
}

// States are hashed by their address but are never considered equal.
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *DocMan<cost_t, str_t>::MakeState(
    typename Doc<cost_t, str_t>::state_fn_t call) {
  // Each node is considered unique so there is no need to hashcons the
  // State node.
  auto node = new State<cost_t, str_t>(call);
  states_.emplace_back(node);
  return node;
}

}  // namespace lpp
