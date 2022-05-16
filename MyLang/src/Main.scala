import scala.util.parsing.input.CharSequenceReader
import mine._

object Main extends App {
  val input = io.Source.fromFile("sample/basicrecurse.rv").mkString
  val in = new CharSequenceReader(input)
  val r = Parser.parseAll(Parser.fun.+, in)
  println(r)
  println()
  println()
  println()
  val PrintStage1 = false
  val PrintStage2 = true
  r.map {r =>
    val ppr = new pp.Preprocessor(r)
    if (PrintStage1)
      for (e <- ppr.output) {
        println()
        println(Format(e))
      }
    val tc = new typecheck.TypeCheck(ppr.output)
    tc.print()
    
    if (PrintStage2)
      for (e <- tc.output) {
        println()
        println(Format(e))
      }
  }
}