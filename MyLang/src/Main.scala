import scala.util.parsing.input.CharSequenceReader
import mine._

object Main extends App {
  val in = new CharSequenceReader(
"""
for:
  | a/Array[T], do/Fun[[T], X] (
    if a.len | (
      do a
      for (tail a) 'do
    )
  )
if:
  | 0, _ [success: 0]
  | _, fun/Fun[[],T] [success: 1, value: fun]
  | 0, _, fun/Fun[[],T] fun
  | _, fun/Fun[[],T], _ fun

tail:
  | a/Array[T] (
    [len: a.len, ptr: a.ptr]
  )

printf/[CString, _*]

println:
  | s/String printf "%s\n" s
  | i/S32 printf "%d\n" i

main:
  | args/Array[String] (
    for args | a/String (
        b: a
        println a
      )
    5
  )
""")
  val r = Parser.fun.+(in)
  println(r)
  println()
  r.map(_.foreach(println))
  println()
  r.map{r =>
    val ppr = new pp.Preprocessor(r)
    for (e <- ppr.output) {
      println()
      println(Format(e))
    }
    val tc = new typecheck.TypeCheck(ppr.output)
    println()
    println()
    println()
    println()
    
    for (e <- tc.output) {
      println()
      println(Format(e))
    }
  }  
}