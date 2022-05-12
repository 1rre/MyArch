package mine
package pp
import Parser._

/**
  * Used to remove lambdas & replace them with by-name calls.
  *
  * @param input
  */

class Preprocessor(input: Seq[Expression]) {
  
  val scope = new RootScope(input, false)

  lazy val output = scope.output
}