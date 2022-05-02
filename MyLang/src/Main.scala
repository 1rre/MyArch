import scala.util.parsing.input.CharSequenceReader
import mine.ToC

object Main extends App {
  val in = new CharSequenceReader(
"""
printf: [CString, _*]

println:
  | s: String printf "%s\n" s
  | i: S32 printf "%d\n" i

main:
  | args: array[string] (
    for args | a: string (
        println a
      )
    5
  )
""")
  val r = mine.Parser.fun.+(in)
  println(r)
  r.map(r => new ToC(r).printC())
}