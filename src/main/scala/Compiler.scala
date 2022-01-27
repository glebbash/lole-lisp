def compile(sourceCode: String, binaryPath: String) =
  val exprs = parse(sourceCode)

  exprs.map(exprToString).foreach(println)
