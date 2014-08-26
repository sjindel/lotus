#include "lpdoc.h"
#include "lpprint.h"

#include <memory>
#include <sstream>
#include <string>

#include "testing/base/public/gunit.h"

using std::unique_ptr;
using std::vector;

namespace lpp {
namespace {

typedef double cost_t;
typedef std::string str_t;
typedef const Doc<cost_t, str_t> doc_t;

// TODO(sjindel): Use template typedefs.
#define Align(doc) Align(DocMan_, doc)
#define Cat DocMan_.MakeCat
#define Choice DocMan_.MakeChoice
#define Fill(c) Fill(DocMan_, c)
#define FlatAlt DocMan_.MakeAlt
#define Hang(i, d) Hang(DocMan_, i, d)
#define Line DocMan_.MakeLine
#define Nest(...) Nest(DocMan_, __VA_ARGS__)
#define State DocMan_.MakeState

class LPPrintTest : public ::testing::Test {
 protected:
  LPPrintTest() {}

  static std::string PrettyPrintUnsafe(doc_t *doc, col_t width) {
    return *PrettyPrint(doc, width);
  }

  doc_t *PrintStateFn(PrintState state) {
    std::ostringstream out;
    out << "(column=" << state.column << " nesting=" << state.nesting
        << " width=" << state.width << ")";
    return Cat(out.str());
  }

  /// Renders the documents in the specified range with either spaces or
  /// newlines between them, such that the first newline has cost 'upper' and
  /// the last newline has cost 'lower'. This encourages the algorithm to flow
  /// the text in a greedy manner.
  template <typename C>
  const Doc<cost_t, str_t> *FlowDocs(const C &docs, cost_t upper,
                                     cost_t lower) {
    cost_t interval = (upper - lower) / static_cast<double>(docs.size());
    doc_t *product = Cat();
    cost_t c = upper;
    for (auto begin = docs.begin(), end = docs.end(); begin != end;
         c -= interval) {
      auto i = begin++;
      if (begin == end)
        product = Cat(product, Cat(*i));
      else
        product = Cat(product, Cat(*i), LineDelim(c, " "));
    }
    return product;
  }

  /// A line that can be rendered as a 'delim' if more efficient.
  template <typename D>
  doc_t *LineDelim(cost_t cost, const D &delim) {
    return Choice(Line(cost), Cat(delim));
  }

  /// A line that is rendered as 'delim' when flattened (Wadler-Leijen style).
  template <typename D>
  doc_t *WLLineDelim(cost_t cost, const D &delim) {
    return FlatAlt(Line(cost), Cat(delim));
  }

  doc_t *PrintState() {
    return DocMan_.MakeState(
        std::bind(&LPPrintTest::PrintStateFn, this, std::placeholders::_1));
  }

  doc_t *LineStartAltFn(doc_t *yes, doc_t *no, const struct PrintState &state) {
    return state.column == 0 ? yes : no;
  }

  doc_t *LineStartAlt(doc_t *yes, doc_t *no) {
    return State([yes, no, this](const struct PrintState &ps) {
      return LineStartAltFn(yes, no, ps);
    });
  }

 protected:
  DocMan<cost_t, str_t> DocMan_;

 private:
  LPPrintTest(const LPPrintTest &) = delete;
  LPPrintTest &operator=(const LPPrintTest &) = delete;
};

TEST_F(LPPrintTest, PrintEmptyTest) {  // nofmt
  EXPECT_EQ("", PrettyPrintUnsafe(Cat(), 80));
}

TEST_F(LPPrintTest, PrintAltTest) {
  EXPECT_EQ("test", PrettyPrintUnsafe(FlatAlt(Cat("test"), Choice()), 80));
}

TEST_F(LPPrintTest, PrintTextTest) {
  EXPECT_EQ("foo", PrettyPrintUnsafe(Cat("foo"), 80));
}

TEST_F(LPPrintTest, PrintLineTest) {
  EXPECT_EQ("\n", PrettyPrintUnsafe(Line(), 80));
}

TEST_F(LPPrintTest, PrintCatTest) {
  EXPECT_EQ("foo\n", PrettyPrintUnsafe(Cat("foo", Line()), 80));
}

TEST_F(LPPrintTest, PrintNestTest) {
  EXPECT_EQ("foo\n  bar",
            PrettyPrintUnsafe(Cat("foo", Nest(2, Line(), "bar")), 80));
}

TEST_F(LPPrintTest, PrintState1Test) {
  EXPECT_EQ("foo (column=4 nesting=0 width=80)",
            PrettyPrintUnsafe(Cat("foo ", PrintState()), 80));
}

TEST_F(LPPrintTest, PrintState2Test) {
  EXPECT_EQ("foo\n  bar (column=6 nesting=2 width=80)",
            PrettyPrintUnsafe(
                Cat("foo", Nest(2, Cat(Line(), "bar ", PrintState()))), 80));
}

TEST_F(LPPrintTest, AlignTest) {
  EXPECT_EQ(
      "foo bar\n"
      "    baz",
      PrettyPrintUnsafe(Cat("foo ", Align(Cat("bar", Line(10.0), "baz"))), 80));
}

TEST_F(LPPrintTest, HangTest) {
  vector<string> text(
      {"  //", "the", "hang", "combinator", "indents", "these", "words"});

  doc_t *doc = Cat("x = 1", Hang(5, FlowDocs(text, 1.0, 0.0)));

  EXPECT_EQ(
      "x = 1  // the hang\n"
      "          combinator\n"
      "          indents these\n"
      "          words",
      PrettyPrintUnsafe(doc, 25));

  EXPECT_EQ(
      "x = 1  // the hang combinator\n"
      "          indents these words",
      PrettyPrintUnsafe(doc, 30));
}

TEST_F(LPPrintTest, HaskellTest) {
  doc_t *doc = Cat("do ", Align(Cat("x <- y", LineDelim(1, "; "), "return z")));
  EXPECT_EQ(
      "do x <- y\n"
      "   return z",
      PrettyPrintUnsafe(doc, 11));
  EXPECT_EQ("do x <- y; return z", PrettyPrintUnsafe(doc, 80));
}

TEST_F(LPPrintTest, AppendTest) {
  EXPECT_EQ("foo bar baz",
            PrettyPrintUnsafe(Cat("foo", " ", "bar", " ", "baz"), 80));
}

TEST_F(LPPrintTest, IndentationTest) {
  EXPECT_EQ(
      "x := foo\n"
      "   y := bar\n"
      "      z := baz\n"
      "return",
      PrettyPrintUnsafe(Cat("x := foo", Nest(3, Line(), "y := bar",
                                             Nest(3, Line(), "z := baz")),
                            Line(), "return"),
                        80));
}

TEST_F(LPPrintTest, Indentation2Test) {
  doc_t *doc = Cat("x := foo", Nest(3, LineDelim(3, "; "), "y := bar",
                                    Nest(3, LineDelim(2, "; "), "z := baz")),
                   LineDelim(1, "; "), "return");
  EXPECT_EQ(
      "x := foo\n"
      "   y := bar\n"
      "      z := baz\n"
      "return",
      PrettyPrintUnsafe(doc, 15));
}

TEST_F(LPPrintTest, SoftLinesTest) {
  vector<string> text{"To",          "organize",   "the",  "world's",
                      "information", "and",        "make", "it",
                      "universally", "accessible", "and",  "useful."};
  EXPECT_EQ(
      "To organize the world's\n"
      "information and make it\n"
      "universally accessible and\n"
      "useful.",
      PrettyPrintUnsafe(FlowDocs(text, 1.0, 0.0), 30));
}

TEST_F(LPPrintTest, FillTest) {
  doc_t *d = Cat(Fill('.'), Line());
  EXPECT_EQ(
      "To.........\n"
      "organize...\n"
      "the........\n"
      "world's....\n"
      "information\n"
      "and........\n"
      "make.......\n"
      "it",
      PrettyPrintUnsafe(Cat("To", d, "organize", d, "the", d, "world's", d,
                            "information", d, "and", d, "make", d, "it"),
                        11));
}

TEST_F(LPPrintTest, LineStartAltTest) {
  // At a line break only if not immediately preceeded by one.
  doc_t *HardLineIfNone = LineStartAlt(Cat(), Line());
  EXPECT_EQ("foo\nbar",
            PrettyPrintUnsafe(Cat("foo", HardLineIfNone, "bar"), 80));
  EXPECT_EQ("foo\nbar",
            PrettyPrintUnsafe(Cat("foo", HardLineIfNone, HardLineIfNone,
                                  HardLineIfNone, "bar"),
                              80));
}

}  // namespace
}  // namespace lpp
