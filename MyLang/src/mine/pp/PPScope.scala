package mine
package pp

import collection.mutable
import util.chaining._
import Parser._
import scala.util.parsing.input.Position

// TODO: This needs re-writing:
//       Eliminate parameterised types (these should be declarable in the parser)
//       Not reorder functions (Although it shoudln't matter, it may?)


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
  def doFun(name: String, af: AnonFun, t: Type): Assignment = {
    val nc = for (c <- af.cases) yield {
      val prs = c.params.collect {
        case TypeParam(name, t) => (name, t)
      }
      val scope = new BlockScope(this, Seq(c.expr), prs, optimise, debug)
      val exp = if (scope.output.length == 1) scope.output.head else Block(scope.output).setPos(c.expr.pos)
      scope.copyFuns()
      FunCase(c.params, exp)
    }
    Assignment(name, AnonFun(nc), t).setPos(af.pos)
  }

  val output: Expression = {
    val x: Expression = expr match {
      case b: Block =>
        val scope = new BlockScope(this, b.exprs, Nil, optimise, debug)
        scope.copyFuns()
        Block(scope.output).setPos(b.pos)
      case af: AnonFun =>
        val name = "anon$" + util.Random.alphanumeric.take(16).mkString
        val as = doFun(name, af, WildCard)
        funs += Assignment(name, as, as.t)
        ByName(name).setPos(af.pos)
      case ef: ExtFun =>
        parent.names += (ef.name, ef.t) -> ef
        ef
      case id@Ident(name) =>
        if (debug)
          for (((n, t), e) <- names if n == name)
            println(s"Option for $n: $t => $e")
        id
      case as@Assignment(name, af: AnonFun, t) =>
        doFun(name, af.setPos(as.pos), t)
      case nf: Assignment =>
        val scope = new OneScope(this, nf.expr, optimise, debug)
        scope.copyFuns()
        val nExpr = scope.output
        parent.names += (nf.name, WildCard) -> nExpr
        Assignment(nf.name, nExpr, nf.t).setPos(nf.pos)
      case fc: FunCall =>
        val funScope = new OneScope(this, fc.fun, optimise, debug)
        funScope.copyFuns()
        val pScopes = for (p <- fc.params) yield {
          val scope = new OneScope(this, p, optimise, debug)
          scope.copyFuns()
          scope.output
        }
        val fs = funScope.output
        FunCall(fs, pScopes).setPos(fc.pos)
      case l: Literal =>
        l
      case ma@MemAccess(on, member) =>
        val scope = new OneScope(this, on, optimise, debug)
        scope.copyFuns()
        val so = scope.output
        MemAccess(so, member).setPos(ma.pos)
      case b: ByName =>
        b
      case StructLiteral(membs) =>
        StructLiteral(membs.map {
          case as@Assignment(name, _: AnonFun, t) =>
            val scope = new OneScope(this, as, optimise, debug)
            scope.copyFuns()
            Assignment(name, ByName(name), t).setPos(as.pos) // Shadow but oh well
          case m =>
            val scope = new OneScope(this, m, optimise, debug)
            val a@Assignment(_,_,_) = scope.output
            scope.copyFuns()
            a
        })
    }
    x.setPos(expr.pos)
  }
}

class BlockScope(parent: BaseScope, exprs: Seq[Expression], args: Seq[(String, Type)], optimise: Boolean, debug: Boolean) extends SubScope(parent, optimise) {
  for ((id, t) <- args)
    names += (id, t) -> Ident(id)
  
  val output: Seq[Expression] = exprs.map { e =>
    val scope = new OneScope(this, e, optimise, debug)
    scope.copyFuns()
    scope.output
  }
}

class RootScope(exprs: Seq[Expression], optimise: Boolean, debug: Boolean) extends BaseScope(optimise) {
  val output: Seq[Expression] = {
    val exps = exprs.map { e =>
      val scope = new OneScope(this, e, optimise, debug)
      scope.copyFuns()
      scope.output
    }
    (exps ++ funs).toSeq
  }

}