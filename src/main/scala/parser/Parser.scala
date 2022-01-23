package parser

import cats.parse.{Parser, Caret}
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, lf, wsp}

enum Atom:
  case Symbol(val name: String)
  case Number(val value: Double)
  case Str(val value: String)

enum SExpr:
  case A(val atom: Atom)
  case E(val expr: List[SExpr])

enum PositionedSExpr:
  case A(val atom: Positioned[Atom])
  case E(val expr: Positioned[List[PositionedSExpr]])

final case class Positioned[T](
    var value: T,
    val start: Caret,
    val end: Caret
)

def parse(input: String) =
  parseOption(input) match
    case Right(res) => res._2
    case Left(err)  => throw new Exception("Parsing failed")

def parseOption = script.parse

val literallyWhitespace = (wsp | lf).rep0
val comment = char(';') *> charsWhile0(_ != '\n') <* lf
val whitespace = literallyWhitespace *>
  comment.repSep0(literallyWhitespace)
  <* literallyWhitespace

val symbol = charWhere(c => c != '(' && c != ')')
  ~ charsWhile0(c => " \t\n()".indexOf(c) == -1)
val number = digit
  ~ (digit | char('_')).rep0
  ~ (char('.') ~ (digit | char('_')).rep).?
val string = // TODO: add escape sequence support
  char('"') *> charsWhile0(_ != '"') <* char('"')
val atom =
  positioned(
    string.string.map(x => Atom.Str(x.substring(1, x.length - 1)))
      | number.string.map(x => Atom.Number(parseNumber(x)))
      | symbol.string.map(x => Atom.Symbol(x))
  ).map(atom => PositionedSExpr.A(atom))

val list: Parser[PositionedSExpr] = defer(
  positioned(
    char('(') *> whitespace *>
      expr.repSep0(whitespace.void)
      <* whitespace <* char(')')
  )
).map(PositionedSExpr.E(_))

def positioned[T](parse: Parser[T]): Parser[Positioned[T]] =
  product10(product01(caret, parse), caret).map { case ((start, value), end) =>
    Positioned(value, start, end)
  }

val expr = atom | list

val script =
  whitespace *> expr.repSep(whitespace).map(_.toList) <* whitespace <* end

// Helpers

def parseNumber(str: String) = str.replaceAll("_", "").toDouble

def withoutPositions(expr: PositionedSExpr): SExpr =
  expr match
    case PositionedSExpr.A(atom) => SExpr.A(atom.value)
    case PositionedSExpr.E(expr) =>
      SExpr.E(expr.value.map(withoutPositions))
