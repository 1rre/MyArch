package mine
package typecheck
import Parser._

class TypeCheck(input: Seq[Expression]) {
  val env = new Env(false, true)
  import env._
  val scope = new RootScope(input)
  def print(scope: Scope = scope): Unit = {
  }
  def psn(sc: Scope): Unit = {
    println()
    println(s"PSN ($sc)")
    for (n <- sc.names) {
      println(n)
    }
  }

  

  val output: Seq[TypedExpression] = {
    scope.checkTypes()
    //psn(scope)
    scope.output
  }
}