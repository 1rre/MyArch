package mine
package pp

import collection.mutable
import util.chaining._
import Parser._

abstract class BaseScope(optimise: Boolean) {
  val names = mutable.Map[(String, Type), Expression]()
  val funs = mutable.Map[String, Expression]()
  private def mergeFun(n: String, e1: Expression, e2: Expression) = ???
  def insertFun(f: Assignment): Unit = {
    if (funs.contains(f.name))
      mergeFun(f.name, funs(f.name), f.expr)
    else funs += f.name -> f.expr
  }
}

abstract class SubScope(parent: BaseScope, optimise: Boolean) extends BaseScope(optimise) {
  def copyFuns(): Unit = {
    for ((name, expr) <- this.funs) {
      if (parent.funs.contains(name))
        sys.error(s"Name collision on $name")
      else parent.insertFun(Assignment(name, expr))
    }
  }
}

class OneScope(parent: BaseScope, expr: Expression, optimise: Boolean) extends SubScope(parent, optimise) {
  for (n <- parent.names)
    names += n
  
  def doFun(name: String, af: AnonFun): Unit = {
    val nc = for (c <- af.cases) yield {
      val prs = c.params.collect {
        case TypeParam(name, t) => (name, t)
      }
      val scope = new BlockScope(this, Seq(c.expr), optimise, prs)
      val exp = if (scope.output.length == 1) scope.output.head else Block(scope.output)
      scope.copyFuns()
      FunCase(c.params, exp)
    }
    insertFun(Assignment(name, AnonFun(nc)))
  }

  val output: Option[Expression] = expr match {
    case b: Block =>
      val scope = new BlockScope(this, b.exprs, optimise, Nil)
      scope.copyFuns()
      Some(Block(scope.output))
    case af: AnonFun =>
      val name = "anon$" + util.Random.alphanumeric.take(16).mkString
      doFun(name, af)
      Some(ByName(name))
    case ef: ExtFun => 
      val sig = ef.signature.zipWithIndex.map {
        case (a,b) => (s"_$b", a)
      }
      parent.names += (ef.name, ParamType(NamedType("Fun"), Seq(TupleType(sig), ef.returns.getOrElse(NamedType("Sub"))))) -> ef
      Some(ef)
    case Ident(name) =>
      for (((n, t), e) <- names if n == name) println(s"Option for $n: $t => $e")
      Some(Ident(name))
    case Assignment(name, af: AnonFun) =>
      doFun(name, af)
      None
    case nf: Assignment =>
      val nExpr = new OneScope(this, nf.expr, optimise).output
      nExpr.map { e =>
        parent.names += (nf.name, WildCard) -> e
        Assignment(nf.name, e)
      }
    case fc: FunCall =>
      val funScope = new OneScope(this, fc.fun, optimise)
      funScope.copyFuns()
      val pScopes = for (p <- fc.params) yield {
        val scope = new OneScope(this, p, optimise)
        scope.copyFuns()
        scope.output
      }
      assert(pScopes.forall(_.isDefined), s"Function call with non-value expression: ${fc.params(pScopes.indexOf(None))}")
      funScope.output.map(fs => FunCall(fs, pScopes.flatten))
    case l: Literal =>
      Some(l)
    case MemAccess(on, member) =>
      val scope = new OneScope(this, on, optimise)
      scope.copyFuns()
      scope.output.map(MemAccess(_, member))
    case b: ByName =>
      Some(b)
    case Struct(membs) =>
      Some(Struct(membs.map {
        case as@Assignment(name, _: AnonFun) =>
          val scope = new OneScope(this, as, optimise)
          scope.copyFuns()
          Assignment(name, ByName(name)) // Shadow but oh well
        case m =>
          val scope = new OneScope(this, m, optimise)
          val Some(a: Assignment) = scope.output
          scope.copyFuns()
          a
      }))
  }
}

class BlockScope(parent: BaseScope, exprs: Seq[Expression], optimise: Boolean, args: Seq[(String, Type)]) extends SubScope(parent, optimise) {
  for ((id, t) <- args)
    names += (id, t) -> Ident(id) // Argument - better way?
  
  val output: Seq[Expression] = exprs.flatMap { e =>
    val scope = new OneScope(this, e, optimise)
    scope.copyFuns()
    scope.output
  }
}

class RootScope(exprs: Seq[Expression], optimise: Boolean) extends BaseScope(optimise) {
  val output: Seq[Expression] = {
    val exps = exprs.flatMap { e =>
      val scope = new OneScope(this, e, optimise)
      scope.copyFuns()
      scope.output
    }
    val fns = funs.map {case (a,b) => Assignment(a,b)}
    (fns ++ exps).toSeq
  }

}