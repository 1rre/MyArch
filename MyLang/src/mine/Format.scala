package mine
import Parser._
import mine.typecheck.Env

object Format {
    
  def apply(t: Type): String = t match {
    case Multiple(t) => s"$t*"
    case ParamType(t, params) => s"$t${params.map(apply).mkString("[", ", ", "]")}"
    case WildCard => "_"
    case TupleType(ts) => ts.map(x => s"${x._1}: ${Format(x._2)}").mkString("[", ", ", "]")
    case NamedType(t) => t
    case EnforcedUnknown => "?"
    case SuperposedType(of) => of.map(Format.apply).mkString(" # ")
    case UnionType(of) => of.map(Format.apply).mkString(" | ")
    case _ => t.toString()
  }
  def apply(p: Param): String = p match {
    case FloatLiteral(value) => s"$value"
    case IntLiteral(value) => s"$value"
    case StringLiteral(value) => s"\"$value\""
    case TypeParam(name, t) => s"$name:${Format(t)}"
    case WildCard => "_"
    case ByName(name) => s"'$name"
    case FunCall(fun, params) =>
      val fFmt = Format(fun)
      val pFmt = params.map(Format.apply).mkString(" ")
      s"$fFmt $pFmt?"
    case EnforcedUnknown => "?"
  }
  def apply(e: Expression): String = e match {
    case ByName(name) => s"'$name"
    case MemAccess(on, member) =>
      val onFmt = Format(on)
      s"$onFmt.$member"
    case AnonFun(cases) =>
      cases.map { case FunCase(p, e) =>
        val pFmt = p.map(Format.apply).mkString(", ")
        val eFmt = Format(e)
        s"  | $pFmt ${eFmt.split("\n").map("    " + _).mkString("\n","\n","")}"
      }.mkString("(\n", "\n", "\n)")
    case FunCall(fun, params) =>
      val fFmt = Format(fun)
      val pFmt = params.map(apply).mkString(" ")
      s"$fFmt $pFmt"
    case Assignment(name, expr, t) =>
      val eFmt = Format(expr)
      val tFmt = Format(t)
      s"$name/$tFmt: $eFmt"
    case Block(exprs) =>
      val eFmt = exprs.map(apply).flatMap(_.split("\n"))
      eFmt.map("  " + _).mkString("(\n", "\n", "\n)")
    case FloatLiteral(value) =>
      s"$value"
    case IntLiteral(value) =>
      s"$value"
    case StringLiteral(value) =>
      s"\"$value\""
    case Ident(name) =>
      name
    case ExtFun(name, t) =>
      val sFmt = Format(t)
      s"$name/$sFmt"
    case StructLiteral(membs) =>
      membs.map(apply).mkString("[", ", ", "]")
  }
  def apply(f: Env#FunScope): String = {
    val pFmt = f.params.map(Format.apply).mkString(", ")
    val eFmt = Format(f.expr)
    s"$pFmt$eFmt"
  }
  def apply(e: Env#TypedExpression): String = e match {
    case e: Env#TypedAnonFun =>
      e.caseScopes.map(Format.apply).mkString("\n| ", "\n| ", "\n")
    case e: Env#TypedByName => s"'${e.base.name}/${Format(e.declaredType.repr)}"
    case e: Env#TypedStringLiteral =>
      s"\"${e.base.value}\"/${Format(e.declaredType.repr)}"
    case e: Env#TypedFloatLiteral =>
      s"${e.base.value}/${Format(e.declaredType.repr)}"
    case e: Env#TypedIntLiteral =>
      s"${e.base.value}/${Format(e.declaredType.repr)}"
    case e: Env#TypedAssignment =>
      s"${e.base.name}/${Format(e.declaredType.repr)}: ${Format(e.subScope.expr)}"
    case e: Env#TypedIdent =>
      s"${e.base.name}/${Format(e.declaredType.repr)}"
    case e: Env#TypedExtFun =>
      s"${e.base.name}/${Format(e.declaredType.repr)}"
    case e: Env#TypedMemAccess => ???
    case e: Env#TypedBlock => e.subScope.exprs.map(x => s"  ${Format(x)}").mkString("(\n", "\n", "\n)")
    case e: Env#TypedFunCall =>
      val fFmt = Format(e.funScope.expr)
      val pFmt = e.paramScopes.map(x => s"(${Format(x.expr)})").mkString(" ")
      s"$fFmt$pFmt"
    case e: Env#TypedStructLiteral => ???
  }

}