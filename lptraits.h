// -*- C++ -*-

#pragma once

#include <cstddef>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>

namespace lpp {

/// The 'cost_t' type parameter to the classes and functions in this library
/// must be ``Copyable'' (they have a copy-constructor defined), ``Assignable''
/// (they have an assignment-constructor defined), ``Descructible'' (they have a
/// destructor defined), ``Hashable'' (std::hash<cost_t> is specialized) and
/// ``Costlike'' (lpp::cost_traits<cost_t> is specialized).
///
/// Is is also recommended (but not required) that it have a move constructor
/// and move assignment operator defined.

/// The 'str_t' type parameter to the classes and functions in this library must
/// be ``Copyable'', ``Assignable'', constructible from string literals and
/// ``Stringlike'' (lpp::string_traits<str_t> is specialized).

template <typename T>
struct cost_traits {
  /// \brief The maximum cost a document can have.
  ///
  /// Must be greater than any other cost and an annihilator of 'add'.
  static T Max() = delete;

  /// \brief The minimum cost a document can have.
  ///
  /// This the default cost for most documents. It must be less than any other
  /// cost and a unit of 'add'.
  static T Zero() = delete;

  /// \brief The cost of a line break.
  static T Line() = delete;

  /// \brief Add two costs.
  ///
  /// \returns the cost associated with concatenating two layouts of the left
  /// and
  /// right costs.
  ///
  /// Should be commutative and associative.
  static T Add(T left, T right) = delete;

  /// \brief Compare two costs.
  ///
  /// \returns 'true' if \p left is less than or equal to \p right.
  static bool Leq(T left, T right) = delete;
};

template <typename T>
struct string_traits {
  /// \brief The output stream type associated with this string type.
  ///
  /// This is optional.
  typedef void ostream_t;

  /// \brief The output string stream type associated with this string type.
  typedef void ostringstream_t;

  /// \brief Return the length of the string.
  static size_t Length(T) = delete;

  /// \brief Create a string by repeating a character \p num times.
  ///
  /// This is optional.
  static T Fill(char, size_t num) = delete;
};

template <>
struct cost_traits<size_t> {
  static size_t Max() { return std::numeric_limits<size_t>::max(); }
  static size_t Zero() { return 0; }
  static size_t Line() { return 0; }

  static size_t Add(size_t x, size_t y) {
    if (x == Max() || y == Max()) return Max();
    return x + y;
  }

  static bool Leq(size_t x, size_t y) {
    return x <= y;
  }
};

template <>
struct cost_traits<double> {
  static double Max() { return 1.0/0.0; }
  static double Zero() { return 0.0; }
  static double Line() { return 0.0; }

  static double Add(double x, double y) {
    return x + y;
  }

  static bool Leq(double x, double y) {
    return x <= y;
  }
};

template <>
struct string_traits<std::string> {
  typedef std::ostream ostream_t;
  typedef std::ostringstream ostringstream_t;

  static size_t Length(const std::string &str) { return str.length(); }

  static std::string Fill(char c, size_t n) { return std::string(n, c); }
};

}  // namespace lpp
