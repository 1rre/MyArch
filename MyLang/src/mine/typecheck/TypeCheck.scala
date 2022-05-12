package mine
package typecheck
import Parser._

class TypeCheck(input: Seq[Expression]) {
  val scope = new RootScope(input, false)
  val output = scope.output
}