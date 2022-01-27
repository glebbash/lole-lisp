import org.junit.Test
import org.junit.Assert.*

import parser._

class ParserTests:
  @Test def parsesEmptyList(): Unit =
    expectEqual(parse("()"), List(expr()))

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
      parse("""
        |(abc1 123 "string")
        |""".stripMargin),
      List(expr(sym("abc1"), num(123), str("string")))
    )

  @Test def skipsWhitespaceAndComments() =
    expectEqual(
      parse("""
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
      parse("(a (b (c () d e f (g h)))) (i) (j k (l m n))"),
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

  // TODO: add parse error tests
  @Test def throwsErrorOnInvalidInput() =
    expectNotToParse("")
    expectNotToParse("(")

// Helpers

def expectParsedSymbol(input: String, name: String) =
  expectEqual(parse(input), List(expr(sym(name))))

def expectParsedNumber(input: String, number: Double) =
  expectEqual(parse(input), List(expr(num(number))))

def expectParsedString(input: String, value: String) =
  expectEqual(parse(input), List(expr(str(value))))

def expectNotToParse(input: String) =
  val res = parseOption(input) match
    case Right(_) => false
    case Left(_)  => true

  assert(res)

def expr(exprs: SExpr*) = SExpr.E(exprs.toList)
def sym(name: String) = SExpr.A(Atom.Symbol(name))
def num(value: Double) = SExpr.A(Atom.Number(value))
def str(value: String) = SExpr.A(Atom.Str(value))

def expectEqual[T](value: T, expected: T) = assertEquals(expected, value)
