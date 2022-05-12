package mine
package pp

import collection.mutable
import util.chaining._
import Parser._
import scala.util.parsing.input.Position

abstract class BaseScope(optimise: Boolean) {
  val names = mutable.Map[(String, Type), Expression]()
  val funs = mutable.Buffer[Assignment]()
  private def mergeFun(n: String, e1: Expression, e2: Expression) = ???
  def insertFun(f: Assignment): Unit = {
    if (funs.contains(f.name))
      mergeFun(f.name, funs.find(_.name == f.name).get, f.expr)
    else funs += f
  }
}

abstract class SubScope(parent: BaseScope, optimise: Boolean) extends BaseScope(optimise) {
  def copyFuns(): Unit = {
    for (as <- this.funs) {
      if (parent.funs.exists(_.name == as.name))
        sys.error(s"Name collision on ${as.name}")
      else parent.insertFun(as)
    }
  }
}

class OneScope(parent: BaseScope, expr: Expression, optimise: Boolean, debug: Boolean) extends SubScope(parent, optimise) {
  for (n <- parent.names)
    names += n
  def doFun(name: String, af: AnonFun): Unit = {
    val nc = for (c <- af.cases) yield {
      val prs = c.params.collect {
        case TypeParam(name, t) => (name, t)
      }
      val scope = new BlockScope(this, Seq(c.expr), prs, optimise, debug)
      val exp = if (scope.output.length == 1) scope.output.head else Block(scope.output).setPos(c.expr.pos)
      scope.copyFuns()
      FunCase(c.params, exp)
    }
    insertFun(Assignment(name, AnonFun(nc)).setPos(af.pos))
  }

  val output: Option[Expression] = (expr match {
    case b: Block =>
      val scope = new BlockScope(this, b.exprs, Nil, optimise, debug)
      scope.copyFuns()
      Some(Block(scope.output).setPos(b.pos))
    case af: AnonFun =>
      val name = "anon$" + util.Random.alphanumeric.take(16).mkString
      doFun(name, af)
      Some(ByName(name).setPos(af.pos))
    case ef: ExtFun => 
      val sig = ef.signature.zipWithIndex.map {
        case (a,b) => (s"_$b", a)
      }
      parent.names += (ef.name, ParamType(NamedType("Fun"), Seq(TupleType(sig), ef.returns.getOrElse(NamedType("Sub"))))) -> ef
      Some(ef)
    case id@Ident(name) =>
      if (debug)
        for (((n, t), e) <- names if n == name)
          println(s"Option for $n: $t => $e")
      Some(id)
    case as@Assignment(name, af: AnonFun) =>
      doFun(name, af.setPos(as.pos))
      None
    case nf: Assignment =>
      val scope = new OneScope(this, nf.expr, optimise, debug)
      scope.copyFuns()
      val nExpr = scope.output
      nExpr.map { e =>
        parent.names += (nf.name, WildCard) -> e
        Assignment(nf.name, e).setPos(nf.pos)
      }
    case fc: FunCall =>
      val funScope = new OneScope(this, fc.fun, optimise, debug)
      funScope.copyFuns()
      val pScopes = for (p <- fc.params) yield {
        val scope = new OneScope(this, p, optimise, debug)
        scope.copyFuns()
        scope.output
      }
      assert(pScopes.forall(_.isDefined), s"Function call with non-value expression: ${fc.params(pScopes.indexOf(None))}")
      funScope.output.map(fs => FunCall(fs, pScopes.flatten).setPos(fc.pos))
    case l: Literal =>
      Some(l)
    case ma@MemAccess(on, member) =>
      val scope = new OneScope(this, on, optimise, debug)
      scope.copyFuns()
      scope.output.map(MemAccess(_, member).setPos(ma.pos))
    case b: ByName =>
      Some(b)
    case Struct(membs) =>
      Some(Struct(membs.map {
        case as@Assignment(name, _: AnonFun) =>
          val scope = new OneScope(this, as, optimise, debug)
          scope.copyFuns()
          Assignment(name, ByName(name)).setPos(as.pos) // Shadow but oh well
        case m =>
          val scope = new OneScope(this, m, optimise, debug)
          val Some(a: Assignment) = scope.output
          scope.copyFuns()
          a
      }))
  }).map(_.setPos(expr.pos))
}

class BlockScope(parent: BaseScope, exprs: Seq[Expression], args: Seq[(String, Type)], optimise: Boolean, debug: Boolean) extends SubScope(parent, optimise) {
  for ((id, t) <- args)
    names += (id, t) -> Ident(id)
  
  val output: Seq[Expression] = exprs.flatMap { e =>
    val scope = new OneScope(this, e, optimise, debug)
    scope.copyFuns()
    scope.output
  }
}

class RootScope(exprs: Seq[Expression], optimise: Boolean, debug: Boolean) extends BaseScope(optimise) {
  val output: Seq[Expression] = {
    val exps = exprs.flatMap { e =>
      val scope = new OneScope(this, e, optimise, debug)
      scope.copyFuns()
      scope.output
    }
    (funs ++ exps).toSeq
  }

}