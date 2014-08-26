// -*- C++ -*-

#pragma once

#include "lpdoc.h"
#include "lpsimple.h"
#include "lptraits.h"

#include <cstddef>
#include <memory>
#include <stdint.h>
#include <string>

namespace lpp {

/// @{
/// Combinators for manipulating documents

/// \brief Change indentation around a document(s).
///
/// Increases the indentation by \p i before rendering the documents, and
/// decreases it by \p i after. If \p i is negative, does the reverse.
template <typename cost_t, typename str_t, typename... Targs>
const Doc<cost_t, str_t> *Nest(DocMan<cost_t, str_t> &D, long i, Targs... docs);

/// \brief Sets the nesting level to the current column.
///
/// This is useful for aligning elements of a structure. For example:
///
/// \code{.cc} MakeCat("foo ", Align("bar", MakeLine(), "baz")) \endcode
///
/// renders as
///
/// \verbatim
/// foo bar
///     baz
/// \endverbatim
///
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Align(DocMan<cost_t, str_t> &D,
                                const Doc<cost_t, str_t> *doc);

/// \brief Relative version of 'Align'.
///
/// Sets the nesting level to the current column + the argument.
///
/// For example:
///
/// \code{.cc}
/// Hang(4, Join(SoftLine(), "the", "hang", "combinator", "indents",
///              "these", "words"));
/// \endcode
///
/// renders (with a pagewidth of 20c) as
///
/// \verbatim
/// the hang combinator
///     indents these
///     words
/// \endverbatim
///
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Hang(DocMan<cost_t, str_t> &D, long indent,
                               const Doc<cost_t, str_t> *doc);

/// Render either \code{.cc} Flatten(x) \endcode if it between the current
/// column and pagewidth, or otherwise \p y.
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *FlattenChoice(DocMan<cost_t, str_t> &D,
                                        const Doc<cost_t, str_t> *x,
                                        const Doc<cost_t, str_t> *y);

/// @{
/// Pretty-printing documents

/// \brief Flattens a 'Doc' onto one line.
///
/// If the document contains hard linebreaks (as are produced by e.g. 'Line()'),
/// the resulting document will be a 'Choice()'. The behavior of this function
/// can be (partially) controlled by the 'FlatAlt(Doc, Doc)' constructor:
///
/// \code{.cc}
/// Render(FlatAlt(x, y)) = Render(x)
/// \endcode
///
/// but
///
/// \code{.cc}
/// Flatten(FlatAlt(x, y)) = y
/// \endcode
///
/// 'FlatAlt' allows us to (for example) express linebreaks that can be
/// formatted either as a newline or a certain delimiter (comma, semicolon,
/// &c.).
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Flatten(DocMan<cost_t, str_t> &D,
                                  const Doc<cost_t, str_t> *);

/// \brief A document that fills the rest of the line with a character.
template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Fill(DocMan<cost_t, str_t> &D, char f);

/// \brief Pretty-print a document.
template <
    typename cost_t, typename str_t,
    typename ostringstream_t = typename string_traits<str_t>::ostringstream_t>
std::unique_ptr<str_t> PrettyPrint(const Doc<cost_t, str_t> *doc,
                                   col_t page_width);

/// @}

}  // namespace lpp

//===----------------------------------------------------------------------===//

namespace lpp {
namespace impl {

/// \brief Representation of column, nesting pairs.
///
/// We pack the (column, nesting) positions into a single machine word for
/// efficiency. For backward compatibility with 32-bit systems, we limit the
/// nesting and column levels to 2^16 (instead of 2^32), which should be far
/// more than sufficient for all uses.
struct render_pos {
  col_t col;
  nest_t nest;

  render_pos() : col(0), nest(0) {}
  render_pos(col_t c, nest_t n) : col(c), nest(n) {}

  inline bool operator==(render_pos y) const {
    return col == y.col && nest == y.nest;
  }

  inline bool operator<(render_pos y) const {
    if (col < y.col) {
      return true;
    } else if (y.col < col) {
      return false;
    } else {
      return nest < y.nest;
    }
  }
};

}  // namespace impl
}  // namespace lpp

namespace std {

template <>
struct hash<::lpp::impl::render_pos> {
  size_t operator()(::lpp::impl::render_pos p) const {
    hash<uint32_t> h;
    return h(static_cast<uint32_t>(p.col) << 16 |
             static_cast<uint32_t>(p.nest));
  }
};

}  // namespace std

namespace lpp {
namespace impl {

/// If it is possible to format a document such that it starts at some
/// column-nesting pair and ends at the pair described by 'end', an object of
/// this class describes what the cost of doing that would be (via 'cost()').
template <typename cost_t>
class LayoutEntry {
 public:
  LayoutEntry(render_pos end, cost_t cost) : end_(end), cost_(cost) {}

  LayoutEntry(const LayoutEntry &other)
      : end_(other.end_), cost_(other.cost_) {}

  render_pos end() const { return end_; }
  cost_t cost() const { return cost_; }

  bool operator<(const LayoutEntry &other) const { return end() < other.end(); }

  ~LayoutEntry() {}

 private:
  LayoutEntry &operator=(const LayoutEntry &) = delete;

  const render_pos end_;
  cost_t cost_;
};

/// An array of 'LayoutEntry<...>'s that describe the possible ways to format a
/// particular document starting at a particular column-nesting pair.
/// 'LotusPrinter::PPrint' is in a sense memoized in two directions -- over the
/// starting column-nesting pairs and over the ending column-nesting pairs.
/// Since we are only interested in renderings that fit within the page-width,
/// we exclude renderings that have a higher column or nesting depth than the
/// page-width.
///
/// Since a 'LayoutResult<...>' is a sparse representation of a row vector in a
/// 'DocMatrix', the entries are stored in lexicographically increasing order
/// over their column-nesting pairs (which are labels of the columns in a
/// 'DocMatrix'). Since it is sparse, inifinite-cost entries are excluded.
template <typename cost_t>
class LayoutResult {
  // We manage the array of 'LayoutEntries' with malloc/free so that we can
  // efficiently resize it after a merge (vector addition) with realloc.
 public:
  typedef LayoutEntry<cost_t> value_type;
  typedef value_type *iterator;
  typedef const value_type *const_iterator;

  LayoutResult() : size_(0), array_(nullptr) {}

  LayoutResult(LayoutResult &&other)
      : size_(other.size_), array_(other.array_) {
    other.size_ = 0;
    other.array_ = nullptr;
  }

  /// \brief Construct a 'LayoutResult' from a single 'LayoutEntry'.
  LayoutResult(render_pos end, cost_t cost)
      : size_(1),
        array_(reinterpret_cast<LayoutEntry<cost_t> *>(
            malloc(sizeof(LayoutEntry<cost_t>)))) {
    new (array_) LayoutEntry<cost_t>(end, std::forward<cost_t>(cost));
  }

  LayoutResult &operator=(LayoutResult &&other) {
    for (size_t i = 0; i < size(); ++i) get(i). ~value_type();
    free(array_);
    size_ = other.size_;
    array_ = other.array_;
    other.size_ = 0;
    other.array_ = nullptr;
    return *this;
  }

  iterator begin() { return array_; }
  iterator end() { return array_ + size_; }
  const_iterator begin() const { return array_; }
  const_iterator end() const { return array_ + size_; }

  /// \brief Access a 'LayoutResult' at a particular render position.
  /// Since the 'LayoutResult's are representations of sparse vectors, this is
  /// not the same as accessing an index in the 'LayoutResult'. Since the
  /// 'LayoutResult's are sparse vectors, if the element is not found it is
  /// assumed to be the zero element of the semirig (i.e., the maximum cost).
  cost_t operator[](render_pos pos) const {
    LayoutEntry<cost_t> zero(pos, cost_traits<cost_t>::Max());
    auto i = std::lower_bound(array(), array() + size(), zero);
    if (i == array() + size()) {
      return zero.cost();
    } else if (i->end() == pos) {
      return i->cost();
    } else {
      return zero.cost();
    }
  }

  /// \brief Multiply a vector by a scalar.
  /// Adds the given cost to every cost in the 'LayoutResult'. Remember that
  /// addition of costs is multiplication over the semirig of 'LayoutEntry's.
  LayoutResult<cost_t> operator*(cost_t cost) const {
    auto *out = reinterpret_cast<LayoutEntry<cost_t> *>(
        malloc(size() * sizeof(LayoutEntry<cost_t>)));
    for (size_t r = 0; r < size(); ++r) {
      new (out + r) LayoutEntry<cost_t>(
          get(r).end(), cost_traits<cost_t>::Add(cost, get(r).cost()));
    }
    return {size(), out};
  }

  /// Merge two 'LayoutResult<...>::type's, chosing the least expensive option
  /// when both \p left and \p right have an equal column-nesting pair, taking
  /// the entry from \p left when the costs are equal. Equivalent to vector
  /// addition over the rig of costs (hence the name).
  LayoutResult<cost_t> operator+(const LayoutResult<cost_t> &other) const {
    size_t l = 0, r = 0, o = 0;

    // We don't care what the array is filled with, since we'll overwrite the
    // positions that matter to us.
    auto *out = reinterpret_cast<LayoutEntry<cost_t> *>(
        malloc((size() + other.size()) * sizeof(LayoutEntry<cost_t>)));

    for (; l != size() && r != other.size(); ++o) {
      if (get(l) < other.get(r)) {
        new (out + o) LayoutEntry<cost_t>(get(l++));
      } else if (other.get(r) < get(l)) {
        new (out + o) LayoutEntry<cost_t>(other.get(r++));
      } else if (cost_traits<cost_t>::Leq(get(l).cost(), other.get(r).cost())) {
        new (out + o) LayoutEntry<cost_t>(get(l++));
        ++r;
      } else {
        new (out + o) LayoutEntry<cost_t>(other.get(r++));
        ++l;
      }
    }

    // Only one of these two loops should happen.
    for (; l != size(); ++o) {
      new (out + o) LayoutEntry<cost_t>(get(l++));
    }
    for (; r != other.size(); ++o) {
      new (out + o) LayoutEntry<cost_t>(other.get(r++));
    }

    // Free up garbage space (and make sure it doesn't confuse us later).
    return {o, reinterpret_cast<LayoutEntry<cost_t> *>(
                   realloc(out, o * sizeof(LayoutEntry<cost_t>)))};
  }

  ~LayoutResult() {
    for (size_t i = 0; i < size(); ++i) get(i). ~value_type();
    free(array_);
  }

 private:
  LayoutResult(const LayoutResult &other) = delete;
  LayoutResult &operator=(const LayoutResult &other) = delete;

  /// Takes ownership of |array|, which should be allocated with
  /// malloc/calloc.
  LayoutResult(size_t size, value_type *array) : size_(size), array_(array) {}

  size_t size() const { return size_; }
  const value_type *array() const { return array_; }

  const value_type &get(size_t pos) const { return array_[pos]; }
  value_type &get(size_t pos) { return array_[pos]; }

  size_t size_;        ///< number of 'LayoutEntry<...>'s in 'array_'.
  value_type *array_;  ///< owned
};

/// The 'DocMatrix' should not hold any 'LayoutResult<...>::type's at a (column,
/// nesting) pair with either column or nesting >= 'page_width'.
template <typename cost_t>
class DocMatrix : public std::unordered_map<render_pos, LayoutResult<cost_t>> {
 public:
  DocMatrix() {}

  ~DocMatrix() {}

 private:
  DocMatrix(const DocMatrix &) = delete;
  DocMatrix &operator=(const DocMatrix &) = delete;
};

template <typename cost_t, typename str_t>
class LotusPrinter {
 public:
  explicit LotusPrinter(col_t page_width) : page_width_(page_width) {}

  typedef const Doc<cost_t, str_t> doc_t;

  /// \returns a reference borrowed from the memo. table.
  const LayoutResult<cost_t> &PPrint(doc_t *doc, render_pos pos) {
    auto &matrix = memos_[doc];
    auto p = matrix.emplace(pos, LayoutResult<cost_t>());
    if (p.second) {
      LayoutResult<cost_t> &new_out = p.first->second;
      new_out = PPrintRaw(doc, pos);
      return new_out;
    }
    return p.first->second;
  }

  /// \brief Reconstruct the 'SimpleDoc' after computing the minimum cost.
  ///
  /// It shall be possible to format \p doc in the given constraints with a
  /// finite cost, and the documents shall already have their costs memoized.
  SimpleDoc<str_t> *Reconstruct(doc_t *doc, render_pos start, render_pos end) {
    switch (doc->kind()) {
      case doc_t::DK_ALT:
        return Reconstruct(
            dynamic_cast<const Alt<cost_t, str_t> *>(doc)->left(), start, end);
      case doc_t::DK_TEXT:
        return new SText<str_t>(
            dynamic_cast<const Text<cost_t, str_t> *>(doc)->text());
      case doc_t::DK_LINE:
        return new SLine<str_t>(start.nest);
      case doc_t::DK_CHOICE: {
        auto *left = dynamic_cast<const Choice<cost_t, str_t> *>(doc)->left();
        auto *right = dynamic_cast<const Choice<cost_t, str_t> *>(doc)->right();

        cost_t left_cost = memos_[left][start][end];
        cost_t right_cost = memos_[right][start][end];

        if (cost_traits<cost_t>::Leq(left_cost, right_cost)) {
          return Reconstruct(left, start, end);
        } else {
          return Reconstruct(right, start, end);
        }
      }
      case doc_t::DK_CAT: {
        auto *left = dynamic_cast<const Cat<cost_t, str_t> *>(doc)->left();
        auto *right = dynamic_cast<const Cat<cost_t, str_t> *>(doc)->right();

        const auto &left_row = memos_[left][start];
        cost_t best_cost = cost_traits<cost_t>::Max();
        render_pos best_mid;
        for (const auto &le : left_row) {
          const auto &right_row = memos_[right][le.end()];
          const cost_t this_cost =
              cost_traits<cost_t>::Add(le.cost(), right_row[end]);
          // This conditional should succeed at least once.
          if (!cost_traits<cost_t>::Leq(best_cost, this_cost)) {
            best_cost = this_cost;
            best_mid = le.end();
          }
        }

        return new SCat<str_t>(Reconstruct(left, start, best_mid),
                               Reconstruct(right, best_mid, end));
      }
      case doc_t::DK_STATE: {
        return Reconstruct(dynamic_cast<const State<cost_t, str_t> *>(doc)
                               ->call()({page_width_, start.nest, start.col}),
                           start, end);
      }
      default:
        return new SEmpty<str_t>();
    }
  }

  /// For any document \p doc and column-nesting pair '{col, nest}', computes
  /// the optimal way to format \p doc between '{col, nest}' and '{col2, nest2}'
  /// with minimal cost for any 'col2' and 'nest2' less or equal to the
  /// page-width. The resulting vector is stored in \p out (which should be
  /// empty).
  LayoutResult<cost_t> PPrintRaw(doc_t *doc, render_pos pos) {
    switch (doc->kind()) {
      case doc_t::DK_FAIL:
        return {};
      case doc_t::DK_EMPTY:
        return {pos, cost_traits<cost_t>::Zero()};
      case doc_t::DK_ALT:
        return PPrintRaw(  // nofmt
            dynamic_cast<const Alt<cost_t, str_t> *>(doc)->left(), pos);
      case doc_t::DK_TEXT: {
        auto *T = dynamic_cast<const Text<cost_t, str_t> *>(doc);
        if (pos.col + T->size() <= page_width_) {
          return {render_pos(pos.col + T->size(), pos.nest),
                  cost_traits<cost_t>::Zero()};
        }
        return {};
      }
      case doc_t::DK_COST:
        return {pos, dynamic_cast<const Cost<cost_t, str_t> *>(doc)->cost()};
      case doc_t::DK_LINE:
        return {render_pos(pos.nest, pos.nest), cost_traits<cost_t>::Line()};
      case doc_t::DK_INDENT: {
        auto *I = dynamic_cast<const Indent<cost_t, str_t> *>(doc);
        if (pos.nest + I->indent() <= page_width_) {
          return {render_pos(pos.col, pos.nest + I->indent()),
                  cost_traits<cost_t>::Zero()};
        }
        return {};
      }
      case doc_t::DK_COLUMN: {
        auto *C = dynamic_cast<const Column<cost_t, str_t> *>(doc);
        if (pos.col + C->column() <= page_width_) {
          return {render_pos(pos.col + C->column(), pos.nest),
                  cost_traits<cost_t>::Zero()};
        }
        return {};
      }
      case doc_t::DK_CHOICE: {
        auto *C = dynamic_cast<const Choice<cost_t, str_t> *>(doc);
        return PPrint(C->left(), pos) + PPrint(C->right(), pos);
      }
      case doc_t::DK_CAT: {
        auto *C = dynamic_cast<const Cat<cost_t, str_t> *>(doc);
        const auto &left = PPrint(C->left(), pos);
        LayoutResult<cost_t> sum;
        for (const auto &le : left) {
          sum = sum + PPrint(C->right(), le.end()) * le.cost();
        }
        return sum;
      }
      case doc_t::DK_STATE:
        return PPrintRaw(
            dynamic_cast<const State<cost_t, str_t> *>(doc)
                ->call()(PrintState(page_width_, pos.nest, pos.col)),
            pos);
      default:
        assert(false && "Invalid constructor for Doc detected in PPrintRaw.");
    }
  }

  const col_t page_width_;
  std::unordered_map<doc_t *, DocMatrix<cost_t>> memos_;
};

template <typename str_t,
          typename ostream_t = typename string_traits<str_t>::ostream_t>
void Display(const SimpleDoc<str_t> *doc, ostream_t &out) {
  bool line = false;
  size_t nest = 0;
  DisplayRec(doc, out, &line, &nest);
}

/// Appends output to \p out (which should be a stream). |line| is whether the
/// previous document rendered ended in a newline; we return whether we ended
/// with a newline.
template <typename str_t,
          typename ostream_t = typename string_traits<str_t>::ostream_t>
void DisplayRec(const SimpleDoc<str_t> *doc, ostream_t &out, bool *newline,
                size_t *nest) {
  switch (doc->kind()) {
    case SimpleDoc<str_t>::EMPTY: {
      break;
    }
    case SimpleDoc<str_t>::TEXT: {
      if (*newline) out << string_traits<str_t>::Fill(' ', *nest);
      out << dynamic_cast<const SText<str_t> *>(doc)->text();
      *newline = false;
      break;
    }
    case SimpleDoc<str_t>::LINE: {
      auto line = dynamic_cast<const SLine<str_t> *>(doc);
      out << "\n";
      *newline = true;
      *nest = line->nest();
      break;
    }
    case SimpleDoc<str_t>::CAT: {
      const auto *mul = dynamic_cast<const SCat<str_t> *>(doc);
      DisplayRec(mul->left(), out, newline, nest);
      DisplayRec(mul->right(), out, newline, nest);
      break;
    }
    default:
      assert(false && "Unhandled constructor for SimpleDoc in Display.");
  }
}

}  // namespace impl

/// \brief Pretty-print a document with the Lotus algorithm.
///
/// \param[out] out the stream to output the rendered document to.
/// \returns whether \p doc could be rendered.
template <typename cost_t, typename str_t,
          typename ostream_t = typename string_traits<str_t>::ostream_t>
bool PrettyPrint(const Doc<cost_t, str_t> *doc, col_t page_width,
                 ostream_t &out) {
  impl::LotusPrinter<cost_t, str_t> lpr(page_width);
  const auto &lr = lpr.PPrint(doc, {0, 0});
  cost_t best_cost = cost_traits<cost_t>::Max();
  const impl::LayoutEntry<cost_t> *best_entry = nullptr;
  for (const auto &le : lr) {
    if (cost_traits<cost_t>::Leq(le.cost(), best_cost)) {
      best_cost = le.cost();
      best_entry = &le;
    }
  }
  if (best_entry) {
    std::unique_ptr<impl::SimpleDoc<str_t>> str(
        lpr.Reconstruct(doc, {0, 0}, best_entry->end()));
    Display(str.get(), out);
    return true;
  } else {
    return false;
  }
}

/// \brief Pretty-print a document with the Lotus algorithm.
///
/// \returns the rendered document, or 'nullptr' if the rendering failed.
template <typename cost_t, typename str_t, typename ostringstream_t>
std::unique_ptr<str_t> PrettyPrint(const Doc<cost_t, str_t> *doc,
                                   col_t page_width) {
  ostringstream_t out;
  if (PrettyPrint(doc, page_width, out))
    return std::unique_ptr<str_t>(new str_t(out.str()));
  else
    return std::unique_ptr<str_t>(nullptr);
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Flatten(DocMan<cost_t, str_t> &D,
                                  const Doc<cost_t, str_t> *doc) {
  switch (doc->kind()) {
    case Doc<cost_t, str_t>::DK_ALT:
      return dynamic_cast<const Alt<cost_t, str_t> *>(doc)->right();
    case Doc<cost_t, str_t>::DK_LINE:
      return D.MakeChoice();
    case Doc<cost_t, str_t>::DK_CAT: {
      auto *C = dynamic_cast<const Cat<cost_t, str_t> *>(doc);
      return D.MakeCat(Flatten(D, C->left()), Flatten(D, C->right()));
    }
    case Doc<cost_t, str_t>::DK_CHOICE: {
      auto *C = dynamic_cast<const Choice<cost_t, str_t> *>(doc);
      return D.MakeChoice(Flatten(D, C->left()), Flatten(D, C->right()));
    }
    case Doc<cost_t, str_t>::DK_STATE: {
      auto call = dynamic_cast<const State<cost_t, str_t> *>(doc)->call();
      return D.MakeState(
          [&D, call](const PrintState &ps) { return Flatten(D, call(ps)); });
    }
    default:
      return doc;
  }
}

template <typename cost_t, typename str_t, typename... Targs>
const Doc<cost_t, str_t> *Nest(DocMan<cost_t, str_t> &D, long i,
                               Targs... docs) {
  return D.MakeCat(D.MakeIndent(i), D.MakeCat(docs...), D.MakeIndent(-i));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Align(DocMan<cost_t, str_t> &D,
                                const Doc<cost_t, str_t> *doc) {
  return D.MakeState([&D, doc](const PrintState &ps) {
    return Nest(D, ps.column - ps.nesting, doc);
  });
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Hang(DocMan<cost_t, str_t> &D, long i,
                               const Doc<cost_t, str_t> *doc) {
  return Align(D, Nest(D, i, doc));
}

template <typename cost_t, typename str_t>
const Doc<cost_t, str_t> *Fill(DocMan<cost_t, str_t> &D, char f) {
  return D.MakeState(  // nofmt
      [&D, f](const PrintState &ps) {
        return ps.width > ps.column
                   ? D.MakeCat(
                         string_traits<str_t>::Fill(f, ps.width - ps.column))
                   : D.MakeCat();
      });
}

}  // namespace lpp
