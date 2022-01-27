import scala.io.Source

@main def process(fileName: String, binaryPath: String = "output"): Unit =
  val input = Source.fromFile(fileName).getLines.mkString("\n")

  compile(input, binaryPath)
