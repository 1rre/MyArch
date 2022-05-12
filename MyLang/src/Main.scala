import scala.util.parsing.input.CharSequenceReader
import mine._

object Main extends App {
  val input = io.Source.fromFile("sample/easy.rv").mkString
  val in = new CharSequenceReader(input)
  val r = Parser.fun.+(in)
  println(r)
  println()
  println()
  println()
  r.map{r =>
    val ppr = new pp.Preprocessor(r)
    /*
    for (e <- ppr.output) {
      println()
      println(Format(e))
    }
    */
    val tc = new typecheck.TypeCheck(ppr.output)
    
    for (e <- tc.output) {
      println()
      println(Format(e))
    }
  }  
}