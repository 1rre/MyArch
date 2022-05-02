package mine

import util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  sealed trait Literal extends Param with Expression
  
  sealed trait Type
  case object U8 extends Type
  case object U16 extends Type
  case object U32 extends Type
  case object U64 extends Type
  case object S8 extends Type
  case object S16 extends Type
  case object S32 extends Type
  case object S64 extends Type
  case object F32 extends Type
  case object F64 extends Type
  case object WildCard extends Type
  case class NamedType(t: String) extends Type
  case class ParamType(t: Type, params: Seq[Type]) extends Type

  def namedTypeName: Parser[Type] = (
    "u8"  ^^^ U8  |||
    "u16" ^^^ U16 |||
    "u32" ^^^ U32 |||
    "u64" ^^^ U64 |||
    "s8"  ^^^ S8  |||
    "s16" ^^^ S16 |||
    "s32" ^^^ S32 |||
    "s64" ^^^ S64 |||
    "f32" ^^^ F32 |||
    "f64" ^^^ F64 |||
    "_" ^^^ WildCard |||
    (identifier ^^ {case a => NamedType(a)})
  )
  def typeName = (
    namedTypeName |||
    (namedTypeName ~ typeParams ^^ {case a~b => ParamType(a,b)})
  )

  sealed trait Expression
  case class Block(exprs: Seq[Expression]) extends Expression
  case class Ident(name: String) extends Expression

  def nonCallExpression: Parser[Expression] =
    literal |||
    block |||
    (identifier ^^ (Ident(_))) |||
    (funCases)
  
  def expression: Parser[Expression] =
    literal |||
    block |||
    (identifier ^^ (Ident(_))) |||
    funCall |||
    (funCases)

  def endl = accept(';') | accept('\n')

  def block = "(" ~> repsep(expression, endl) <~ ")" ^^ (Block(_))

  def identifier = "[a-zA-Z_][a-zA-Z_0-9]*".r

  case class Multiple(t: Type) extends Type

  def typeList: Parser[Seq[Type]] =
    (typeName <~ ",") ~ typeList ^^ (a => a._1 +: a._2) |||
    (typeName <~ "*" ^^ (Multiple(_)) ^^ (Seq(_))) |||
    typeName ^^ (Seq(_))

  def typeParams = "[" ~> typeList <~ "]"
  case class AnonFun(cases: Seq[FunCase]) extends Expression
  case class FunCase(params: Seq[Param], expr: Expression)
  def funCases =  ("|" ~> repsep(param, ",") ~ expression ^^ {case a~b => FunCase(a,b)}).+ ^^ (AnonFun(_))
  case class NamedFun(name: String, expr: Expression) extends Expression
  case class ExtFun(name: String, signature: Seq[Type], returns: Option[Type]) extends Expression
  def fun =
    identifier ~ (":" ~> expression) ^^ (x => NamedFun(x._1, x._2)) |
    identifier ~ (":" ~> typeParams ~ (":" ~> typeName).?) ^^ (x => ExtFun(x._1, x._2._1, x._2._2))

  case class FunCall(fun: Expression, params: Seq[Expression]) extends Expression
  def __funcallpf(a: Expression, b: Expression) = a match {
    case a: FunCall => FunCall(a.fun, a.params :+ b)
    case a => FunCall(a, Seq(b))
  }
  def funCall =
    chainl1((identifier ^^ (Ident(_))), nonCallExpression, not(accept('\n')) ^^^ (__funcallpf(_,_)))
  //identifier ~ (repsep(expression, not(accept('\n')))) ^^ {case a~b => FunCall(a,b)}

  sealed trait Param
  def param = typeParam | literal

  case class TypeParam(name: String, t: Type) extends Param
  def typeParam = identifier ~ (":" ~> typeName) ^^ (x => TypeParam(x._1, x._2))

  case class IntLiteral(value: BigInt) extends Literal
  def intLiteral = "[0-9]+".r ^^ (BigInt(_)) ^^ (IntLiteral(_))
  case class FloatLiteral(value: BigDecimal) extends Literal
  def floatLiteral = "[0-9]+.[0-9]+".r ^^ (BigDecimal(_)) ^^ (FloatLiteral(_))
  case class StringLiteral(value: String) extends Literal
  def stringLiteral = "[\"]".r ~> "[^\"]+".r <~ "[\"]".r ^^ (StringLiteral(_))

  def literal = intLiteral | floatLiteral | stringLiteral
}