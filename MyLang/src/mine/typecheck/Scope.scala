package mine
package typecheck

import Parser._
import collection.mutable
import mutable.Buffer

object Unifier {
  def anyFun = new Parameterised("Fun", Unknown, Unknown)
}
sealed trait Unifier {
  def restrict(t: Unifier): Unifier
}
class TStruct(knownMembers: (String, Unifier)*) extends Unifier {
  def restrict(t: Unifier): Unifier = ???
}
class Parameterised(name: String, params: Unifier*) extends Unifier {
  // This may need binds to be useful?
  def restrict(t: Unifier): Unifier = ???
}
class Union(opts: Unifier*) extends Unifier {
  def restrict(t: Unifier): Unifier = ???
}
class Superposition(of: Unifier*) extends Unifier {
  def restrict(t: Unifier): Unifier = ???
}
case object Unknown extends Unifier {
  def restrict(t: Unifier): Unifier = ???
}
case object UF32 extends Unifier {
  def restrict(t: Unifier): Unifier = ???
}
case object UF64 extends Unifier {
  def restrict(t: Unifier): Unifier =
    if (t != UF64) throw new Throwable
    else UF64
}

object TypedExpression {
  def apply(e: Expression): TypedExpression = e match {
    case ma: MemAccess =>
      TypedMemAccess(ma)
    case fc: FunCall =>
      TypedFunCall(fc)
    case as: Assignment =>
      TypedAssignment(as)
    case fl: FloatLiteral =>
      TypedFloatLiteral(fl)
    case il: IntLiteral =>
      TypedIntLiteral(il)
    case sl: StringLiteral =>
      TypedStringLiteral(sl)
    case bn: ByName =>
      TypedByName(bn)
    case af: AnonFun =>
      TypedAnonFun(af)
    case bl: Block =>
      TypedBlock(bl)
    case st: Struct =>
      TypedStruct(st)
    case ex: ExtFun =>
      TypedExtFun(ex)
    case id: Ident =>
      TypedIdent(id)
  }
}
sealed trait TypedExpression {
  var t: Unifier
}
sealed trait NameSource {this: TypedExpression => }
case class TypedMemAccess(a: MemAccess, var t: Unifier = Unknown) extends TypedExpression
case class TypedFunCall(a: FunCall, var t: Unifier = Unknown) extends TypedExpression
case class TypedAssignment(a: Assignment, var t: Unifier = Unknown) extends TypedExpression with NameSource
case class TypedFloatLiteral(a: FloatLiteral, var t: Unifier = Unknown) extends TypedExpression
case class TypedIntLiteral(a: IntLiteral, var t: Unifier = Unknown) extends TypedExpression
case class TypedStringLiteral(a: StringLiteral, var t: Unifier = Unknown) extends TypedExpression
case class TypedByName(a: ByName, var t: Unifier = Unknown) extends TypedExpression
case class TypedAnonFun(a: AnonFun, var t: Unifier = Unknown) extends TypedExpression
case class TypedBlock(a: Block, var t: Unifier = Unknown) extends TypedExpression
case class TypedStruct(a: Struct, var t: Unifier = Unknown) extends TypedExpression
case class TypedExtFun(a: ExtFun, var t: Unifier = Unknown) extends TypedExpression with NameSource
case class TypedIdent(a: Ident, var t: Unifier = Unknown) extends TypedExpression

abstract class Scope(optimise: Boolean, debug: Boolean) {
  val names: Map[String, NameSource]

}

class SubScope(parent: Scope, optimise: Boolean, debug: Boolean) extends Scope(optimise, debug) {
  val names = parent.names
}

class RootScope(input: Seq[Expression], optimise: Boolean, debug: Boolean = false) extends Scope(optimise, debug) {
  val types = input.map(i => TypedExpression(i))
  val names = types.collect {
    case as: TypedAssignment => as.a.name -> as
    case ex: TypedExtFun => ex.a.name -> ex
  }.toMap
  def checkTypes(): Unit = {
    for (t <- types) {
      println(t)
    }
    for ((name, as) <- names) {
      println(s"$name -> $as")
    }
  }
  val output = input
}

/*
input.foreach {
      case ExtFun(name, signature, returns) => 
      case Block(exprs) =>
      case AnonFun(cases) =>
      case FunCall(fun, params) =>
      case Assignment(name, expr) =>
      case Struct(membs) =>
      case Ident(name) =>
      case MemAccess(on, member) =>
        // TODO: Ensure that "on" has a member "member"
      case FloatLiteral(value) =>
        // I don't think we need to do anything here, as we're not linking anything
      case IntLiteral(value) =>
        // I don't think we need to do anything here, as we're not linking anything
      case StringLiteral(value) =>
        // I don't think we need to do anything here, as we're not linking anything
      case ByName(name) =>
        // I think this may necessitate "name" linking to a function in this context?
        ???
    }
*/
