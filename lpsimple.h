// -*- C++ -*-

#pragma once

#include "lpdoc.h"
#include "lprc.h"

#include <cstddef>

namespace lpp {
namespace impl {

/// @{
/// 'SimpleDoc' classes and implementation.

/// \brief 'SimpleDoc' represents formatted text.
template <typename str_t>
class SimpleDoc {
 public:
  enum Kind {
    EMPTY,  ///< An empty document.
    TEXT,   ///< A piece of text, with no newlines.
    LINE,   ///< A line break.
    CAT     ///< Concatenation of two 'SimpleDoc's.
  };

  // TODO(sjindel): Devirtualize.
  virtual Kind kind() const = 0;

  virtual ~SimpleDoc() {}

 protected:
  SimpleDoc() {}

 private:
  SimpleDoc(const SimpleDoc &) = delete;
  SimpleDoc &operator=(const SimpleDoc &) = delete;
};

/// \brief An empty 'SimpleDoc'.
template <typename str_t>
class SEmpty : public SimpleDoc<str_t> {
 public:
  typedef SimpleDoc<str_t> super;

  SEmpty() {}

  typename super::Kind kind() const override { return super::EMPTY; }

  virtual ~SEmpty() {}

 private:
  SEmpty(const SEmpty &) = delete;
  SEmpty &operator=(const SEmpty &) = delete;
};

/// \brief A piece of text.
template <typename str_t>
class SText : public SimpleDoc<str_t> {
 public:
  typedef SimpleDoc<str_t> super;

  SText(const str_t &text) : text_(text) {}

  typename super::Kind kind() const override { return super::TEXT; }

  const str_t &text() const { return text_; }

  virtual ~SText() {}

 private:
  SText(const SText &) = delete;
  SText &operator=(const SText &) = delete;

  const str_t text_;
};

/// \brief A line break followed by its continutation.
///
/// Records the indentation level of the line that follows it.
template <typename str_t>
class SLine : public SimpleDoc<str_t> {
 public:
  typedef SimpleDoc<str_t> super;

  explicit SLine(nest_t nest) : nest_(nest) {}

  typename super::Kind kind() const override { return super::LINE; }

  /// The indentation of the following line.
  nest_t nest() const { return nest_; }

  virtual ~SLine() {}

 private:
  SLine(const SLine &) = delete;
  SLine &operator=(const SLine &) = delete;

  const nest_t nest_;
};

/// \brief Concatenation of two 'SimpleDoc's
template <typename str_t>
class SCat : public SimpleDoc<str_t> {
 public:
  typedef SimpleDoc<str_t> super;

  /// Takes ownership of its arguments.
  SCat(super *left, super *right) : left_(left), right_(right) {}

  typename super::Kind kind() const override { return super::CAT; }

  /// Returns pointer borrowed from this object.
  const super *left() const { return left_.get(); }

  /// Returns pointer borrowed from this object.
  const super *right() const { return right_.get(); }

  virtual ~SCat() {}

 private:
  SCat(const SCat &) = delete;
  SCat &operator=(const SCat &) = delete;

  const std::unique_ptr<const super> left_;
  const std::unique_ptr<const super> right_;
};

}  // namespace impl
}  // namespace lpp
