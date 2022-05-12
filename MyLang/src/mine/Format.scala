package mine
import Parser._

object Format {
    
  def apply(t: Type): String = t match {
    case Multiple(t) => s"$t*"
    case ParamType(t, params) => s"${Format(t)}${params.map(apply).mkString("[", ", ", "]")}"
    case WildCard => "_"
    case TupleType(ts) => ts.map(x => s"${x._1}: ${Format(x._2)}").mkString("[", ", ", "]")
    case NamedType(t) => t
    case _ => t.toString()
  }
  def apply(p: Param): String = p match {
    case FloatLiteral(value) => s"$value"
    case IntLiteral(value) => s"$value"
    case StringLiteral(value) => s"\"$value\""
    case TypeParam(name, t) => s"$name:${Format(t)}"
    case WildCard => "_"
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
    case Assignment(name, expr) =>
      val eFmt = Format(expr)
      s"$name: $eFmt"
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
    case ExtFun(name, signature, returns) =>
      val sFmt = signature.map(apply)
      val rFmt = returns.map(apply).getOrElse("None")
      s"$name : ${sFmt.mkString(", ")} : $rFmt"
    case Struct(membs) =>
      membs.map(apply).mkString("[", ", ", "]")
  }

}