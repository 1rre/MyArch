package mine
package typecheck
import Parser._

class TypeCheck(input: Seq[Expression]) {
  val scope = new RootScope(input, false, true)
  val output = {
    scope.checkTypes()
    scope.output
  }
}