import scala.io.Source

@main def process(fileName: String): Unit =
  val input = Source.fromFile(fileName).getLines.mkString("\n")

  parse(input).map(exprToString).foreach(println)
