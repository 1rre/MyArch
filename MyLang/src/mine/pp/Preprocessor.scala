package mine
package pp
import Parser._

/**
  * Used to remove lambdas & replace them with by-name calls.
  *
  * @param input
  */

class Preprocessor(input: Seq[Expression], optimise: Boolean = false, debug: Boolean = false) {
  
  val scope = new RootScope(input, optimise, debug)

  lazy val output = scope.output
}