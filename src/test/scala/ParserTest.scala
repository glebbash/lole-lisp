import org.junit.Test
import org.junit.Assert.*

import parser._
import cats.parse.Caret

class Parser:
  @Test def parsesEmptyList(): Unit =
    expectEqual(parse0("()"), List(expr()))

  @Test def parsesSymbols() =
    expectParsedSymbol("(a)", "a")
    expectParsedSymbol("(abc)", "abc")

  @Test def parsesNumbers() =
    expectParsedNumber("(0)", 0)
    expectParsedNumber("(10)", 10)
    expectParsedNumber("(10.0)", 10.0)
    expectParsedNumber("(0.10)", 0.10)
    expectParsedNumber("(100_000.000_000)", 100_000.000_000)

  @Test def parsesStrings() =
    expectParsedString(
      """
      |("The quick brown fox jumps over the lazy dog.")
      |""".stripMargin,
      "The quick brown fox jumps over the lazy dog."
    )
    {
      // TODO: uncomment when string quorte escaping is implemented
      // expectParsedString(
      //   '("\\"escaped quotes supported\\"")',
      //   '"\\"escaped quotes supported\\""'
      // )
    }

  @Test def parsesListsWithMultipleAtoms() =
    expectEqual(
      parse0("""
        |(abc1 123 "string")
        |""".stripMargin),
      List(expr(sym("abc1"), num(123), str("string")))
    )

  @Test def skipsWhitespaceAndComments() =
    expectEqual(
      parse0("""
        |(a             b               c) ;comment
        |; comment
        |(
        |  1 ; comment
        |  2 ; comment ; comment ; comment
        |  3 ;; comment
        |)
        |""".stripMargin),
      List(
        expr(sym("a"), sym("b"), sym("c")),
        expr(num(1), num(2), num(3))
      )
    )

  @Test def parsesComplexExpressions() =
    expectEqual(
      parse0("(a (b (c () d e f (g h)))) (i) (j k (l m n))"),
      List(
        expr(
          sym("a"),
          expr(
            sym("b"),
            expr(
              sym("c"),
              expr(),
              sym("d"),
              sym("e"),
              sym("f"),
              expr(sym("g"), sym("h"))
            )
          )
        ),
        expr(sym("i")),
        expr(sym("j"), sym("k"), expr(sym("l"), sym("m"), sym("n")))
      )
    )

  @Test def parsesHelloWorld() =
    expectEqual(
      parse0("""
        |;; Hello World example
        |(llvm/target-triple "x86_64-pc-linux-gnu") ; optional
        |(external-fn puts (&i8) i32)
        |(fn main () i32
        |  (puts "Hello World!")
        |  (i32 0)
        |)
        |""".stripMargin),
      List(
        expr(sym("llvm/target-triple"), str("x86_64-pc-linux-gnu")),
        expr(
          sym("external-fn"),
          sym("puts"),
          expr(sym("&i8")),
          sym("i32")
        ),
        expr(
          sym("fn"),
          sym("main"),
          expr(),
          sym("i32"),
          expr(sym("puts"), str("Hello World!")),
          expr(sym("i32"), num(0))
        )
      )
    )

  @Test def parsesHelloWorldWithCorrectPositions() =
    expectEqual(
      parse("""
        |;; Hello World example
        |(llvm/target-triple "x86_64-pc-linux-gnu") ; optional
        |(external-fn puts (&i8) i32)
        |(fn main () i32
        |  (puts "Hello World!")
        |  (i32 0)
        |)
        |""".stripMargin),
      List(
        exprAt(
          List(
            symAt("llvm/target-triple", Caret(2, 1, 25), Caret(2, 19, 43)),
            strAt("x86_64-pc-linux-gnu", Caret(2, 20, 44), Caret(2, 41, 65))
          ),
          Caret(2, 0, 24),
          Caret(2, 42, 66)
        ),
        exprAt(
          List(
            symAt("external-fn", Caret(3, 1, 79), Caret(3, 12, 90)),
            symAt("puts", Caret(3, 13, 91), Caret(3, 17, 95)),
            exprAt(
              List(symAt("&i8", Caret(3, 19, 97), Caret(3, 22, 100))),
              Caret(3, 18, 96),
              Caret(3, 23, 101)
            ),
            symAt("i32", Caret(3, 24, 102), Caret(3, 27, 105))
          ),
          Caret(3, 0, 78),
          Caret(3, 28, 106)
        ),
        exprAt(
          List(
            symAt("fn", Caret(4, 1, 108), Caret(4, 3, 110)),
            symAt("main", Caret(4, 4, 111), Caret(4, 8, 115)),
            exprAt(List(), Caret(4, 9, 116), Caret(4, 11, 118)),
            symAt("i32", Caret(4, 12, 119), Caret(4, 15, 122)),
            exprAt(
              List(
                symAt("puts", Caret(5, 3, 126), Caret(5, 7, 130)),
                strAt("Hello World!", Caret(5, 8, 131), Caret(5, 22, 145))
              ),
              Caret(5, 2, 125),
              Caret(5, 23, 146)
            ),
            exprAt(
              List(
                symAt("i32", Caret(6, 3, 150), Caret(6, 6, 153)),
                numAt(0, Caret(6, 7, 154), Caret(6, 8, 155))
              ),
              Caret(6, 2, 149),
              Caret(6, 9, 156)
            )
          ),
          Caret(4, 0, 107),
          Caret(7, 1, 158)
        )
      )
    )

// TODO: add parse error tests
// @Test def throwsErrorOnInvalidInput() =
//   expectEqual(parseOption(""), Left("error"))

// Helpers

def expectParsedSymbol(input: String, name: String) =
  expectEqual(parse0(input), List(expr(sym(name))))

def expectParsedNumber(input: String, number: Double) =
  expectEqual(parse0(input), List(expr(num(number))))

def expectParsedString(input: String, value: String) =
  expectEqual(parse0(input), List(expr(str(value))))

def expr(exprs: SExpr*) = SExpr.E(exprs.toList)
def sym(name: String) = SExpr.A(Atom.Symbol(name))
def num(value: Double) = SExpr.A(Atom.Number(value))
def str(value: String) = SExpr.A(Atom.Str(value))

def exprAt(exprs: List[PositionedSExpr], start: Caret, end: Caret) =
  PositionedSExpr.E(Positioned(exprs, start, end))
def symAt(name: String, start: Caret, end: Caret) =
  PositionedSExpr.A(Positioned(Atom.Symbol(name), start, end))
def numAt(value: Double, start: Caret, end: Caret) =
  PositionedSExpr.A(Positioned(Atom.Number(value), start, end))
def strAt(value: String, start: Caret, end: Caret) =
  PositionedSExpr.A(Positioned(Atom.Str(value), start, end))

def parse0(input: String) = parse(input).map(withoutPositions)

def expectEqual[T](value: T, expected: T) = assertEquals(expected, value)
