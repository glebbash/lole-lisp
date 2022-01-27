def exprToString(expr: SExpr): String =
  expr match
    case SExpr.Atom(value) =>
      value match
        case Value.Symbol(name)  => name
        case Value.Number(value) => value.toInt.toString
        case Value.Str(value)    => '"' + value + '"'
    case SExpr.Expr(expr) => '(' + expr.map(exprToString).mkString(" ") + ')'
