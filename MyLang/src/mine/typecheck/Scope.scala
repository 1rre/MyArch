package mine
package typecheck

import Parser._
import collection.mutable
import mutable.Buffer

abstract class Scope {
  val names = mutable.Map[String, Buffer[Type]]()
  val binds = mutable.Map[String, Seq[Type]]()
  def addName(n: String, expr: Expression): Unit = {
    addName(n, possibleTypes(expr))
  }
  def addName(n: String, types: Seq[Type]): Unit = {
    names.getOrElseUpdate(n, Buffer()) ++= types
  }
  def ptr(to: Type) = ParamType(NamedType("Ptr"), Seq(to))
  def fun(params: Seq[Type], returns: Type) =
    ParamType(NamedType("Fun"), Seq(TupleType(params.zipWithIndex.map(x => s"_${x._1}" -> x._1))))
  def struct(membs: (String, Type)*) = {
    TupleType(membs)
  }
  def flatTypes(ts: Seq[Seq[Type]]): Seq[Seq[Type]] =
    if (ts.isEmpty) Nil
    else {
      val tl = flatTypes(ts.tail)
      ts.head.flatMap(x => tl.map(x +: _))
    }
  def reduceFunCall(funTypes: Seq[Type], params: Seq[Seq[Type]]): Seq[Type] = {
    println(s"Valid fun types: $funTypes")
    println(s"Valid param types: $params")
    ???
  }
  def possibleTypes(t: Type): Seq[Type] = t match {
    case NamedType(t) =>
      binds(t)
    case TupleType(ts) =>
      // TODO: Resolve tuple types, eg [len: x] and [len: y, ptr: z]
      ???
    case Multiple(t) =>
      possibleTypes(t).map(ptr)
    case ParamType(t, params) =>
      val pt = flatTypes(params.map(possibleTypes))
      possibleTypes(t).flatMap(tx => pt.map(p => ParamType(tx, p)))
    case _ => Seq(t)
  }
  def possibleTypes(p: Param): Seq[Type] = p match {
    case FloatLiteral(value) => Seq(F32, F64)
    case IntLiteral(value) => Seq(U8, U16, U32, U64, S8, S16, S32, S64)
    case StringLiteral(value) => Seq (
      TupleType(Seq("len" -> U32, "ptr" -> ptr(U8))), // Array type string
      ptr(U8) // C String
    )
    case TypeParam(_, t) =>
      possibleTypes(t)
    case WildCard => Seq(WildCard)
  }
  def possibleTypes(e: Expression): Seq[Type] =  e match {
    case FloatLiteral(value) => Seq(F32, F64)
    case IntLiteral(value) => Seq(U8, U16, U32, U64, S8, S16, S32, S64)
    case StringLiteral(value) => Seq (
      TupleType(Seq("len" -> U32, "ptr" -> ptr(U8))), // Array type string
      ptr(U8) // C String
    )
    case Block(exprs) => ???
    case ByName(name) => ???
    case AnonFun(cases) =>
      cases.flatMap { c =>
        val pTypes = c.params.map(p => possibleTypes(p))
        pTypes.flatMap { p =>
          val scope = new OneScope(this, c.expr)
          for (TypeParam(n, t) <- c.params) {
            println(s"TODO: Add type for $n ($t)")
          }
          scope.possibleTypes(c.expr).map(fun(p, _))
        }
      }
    case FunCall(fun, params) =>
      val funNameScope = new OneScope(this, fun)
      val funTypes = funNameScope.possibleTypes(fun)
      val pTypes = flatTypes(params.map(possibleTypes))
      reduceFunCall(funTypes, pTypes)

    case Struct(membs) => ???
    case Assignment(name, expr) =>
      // Assignments shouldn't return anything, but for convenience I'll return the expr's return type
      val scope = new OneScope(this, expr)
      val pt = possibleTypes(expr)
      addName(name, pt)
      pt
    case MemAccess(on, member) => ???
    case ExtFun(name, signature, returns) => ???
    case Ident(name) =>
      names.get(name).map(_.toSeq).getOrElse {
        println(s"Name $name not yet declared. Using wildcard.")
        Seq(WildCard)
      }
  }
}

abstract class SubScope(parent: Scope) extends Scope {
  for (n <- parent.names) names += n
  for (n <- parent.binds) binds += n
}

class OneScope(parent: Scope, input: Expression) extends Scope {
  
}

class RootScope(input: Seq[Expression], optimise: Boolean) extends Scope {
  binds += "String" -> Seq (
      TupleType(Seq("len" -> U32, "ptr" -> ptr(U8))), // Array type string
      ptr(U8) // C String
    )
  val output = input
  for (exp <- input) {
    println(possibleTypes(exp))
  }
}