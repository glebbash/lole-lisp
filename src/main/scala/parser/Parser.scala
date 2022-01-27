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

def parse(input: String) =
  parseOption(input) match
    case Right((_, result)) => result
    case Left(err)          => throw new Exception("Parsing failed")

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
val string = char('"') *> charsWhile0(_ != '"') <* char('"')
val atom = (string.string.map(x => Atom.Str(x.substring(1, x.length - 1)))
  | number.string.map(x => Atom.Number(parseNumber(x)))
  | symbol.string.map(x => Atom.Symbol(x))).map(atom => SExpr.A(atom))

val list: Parser[SExpr] = defer(
  char('(') *> whitespace *>
    expr.repSep0(whitespace.void)
    <* whitespace <* char(')')
).map(SExpr.E(_))

val expr = atom | list

val script =
  whitespace *> expr.repSep(whitespace).map(_.toList) <* whitespace <* end

// Helpers

def parseNumber(str: String) = str.replaceAll("_", "").toDouble
