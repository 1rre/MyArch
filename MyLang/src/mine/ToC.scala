package mine
import Parser._

class ToC(expressions: Seq[Expression]) {
  sealed trait CType
  case class CUnsignedT(size: Int) extends CType
  case class CSignedT(size: Int) extends CType
  case class CFloatT(size: Int) extends CType
  case class CPtrT(to: CType) extends CType
  def CString = CPtrT(CUnsignedT(8))
  case class CStructT(members: Seq[(String, CType)]) extends CType
  case object CVoidT extends CType
  case object CVarArgsT extends CType
  case class CFunT(returns: CType, args: Seq[CType]) extends CType
  sealed trait CExpr {
    val t: CType
  }

  def typeToC(t: Type): CType = t match {
    case U8 => CUnsignedT(8)
    case U16 => CUnsignedT(16)
    case U32 => CUnsignedT(32)
    case U64 => CUnsignedT(64)
    case S8 => CSignedT(8)
    case S16 => CSignedT(16)
    case S32 => CSignedT(32)
    case S64 => CSignedT(64)
    case F32 => CFloatT(32)
    case F64 => CFloatT(64)
    case WildCard => CPtrT(CVoidT)
    case Multiple(t) if t == WildCard => CVarArgsT
    case Multiple(t) => CPtrT(typeToC(t))
    case NamedType(t) => sys.error(s"TODO: Resolve type name $t") // Todo: Resolve type name
    case ParamType(t, params) => sys.error(s"TODO: Type Parameters for $t[$params]")
  }

  class CValue(_t: Type, v: Literal) extends CExpr {
    val t = typeToC(_t)
  }
  class CFnCall(fun: Expression, params: Seq[Expression]) extends CExpr {
    val t = sys.error(s"Function call on $fun not possible")
  }
  class CBlock(exprs: Seq[Expression]) extends CExpr {
    val cExprs = exprs.map(exprToC)
    val t = cExprs.lastOption.map(_.t).getOrElse(CVoidT)
  }
  class CFun(name: String, value: Expression) extends CExpr {
    val cExpr = exprToC(value)
    val t = cExpr.t
  }
  class CFloat(value: BigDecimal) extends CExpr {
    val t = CFloatT(64)
  }
  class CInt(value: BigInt) extends CExpr {
    val t = CSignedT(32)
  }
  class CString(value: String) extends CExpr {
    val t = CString
  }
  class CAnonFun(cases: Seq[FunCase]) extends CExpr {
    val t = ???
  }
  class CIdent(name: String) extends CExpr {
    val t = sys.error(s"Ident lookup unsupported for $name")
  }
  class CExternFun(name: String, signature: Seq[CType], returns: CType) extends CExpr {
    val t = CFunT(returns, signature)
  }
  def exprToC(e: Expression): CExpr = e match {
    case Block(exprs) => new CBlock(exprs)
    case ExtFun(name, signature, returns) =>
      new CExternFun(name, signature map typeToC, returns map typeToC getOrElse CVoidT)
    case FunCall(fun, exprs) => new CFnCall(fun, exprs)
    case FloatLiteral(value) => new CFloat(value)
    case IntLiteral(value) => new CInt(value)
    case StringLiteral(value) => new CString(value)
    case AnonFun(cases) => new CAnonFun(cases)
    case NamedFun(name, expr) => new CFun(name, expr)
    case Ident(name) => new CIdent(name)
  }
  val exprs = for (expr <- expressions) yield {
    exprToC(expr)
  }
  def printC(): Unit = {
    for (expr <- exprs) {
      println((expr, expr.t))
    }
  }
}