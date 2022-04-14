import es.tmoor.dynarch.Parser
import es.tmoor.dynarch.Interpreter
import es.tmoor.dynarch.Assembler
import es.tmoor.dynarch.Disassembler

object Main extends App {
  val input = new util.parsing.input.CharSequenceReader(
"""
5.0
: x0
x0
: y0
i64 @ y0
0
: x0
f64 @ [x0]
: x1
""")
  val result = Parser.operation.+(input)
  println(result)
  result.map {l =>
    println("Interpreting")
    (new Interpreter).interpret(l)
  }
}