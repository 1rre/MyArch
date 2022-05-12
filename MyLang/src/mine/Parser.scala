package mine

import util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

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
  case object WildCard extends Type with Param
  // TupleType is actually an anonymous named type
  case class TupleType(ts: Seq[(String, Type)]) extends Type
  case class NamedType(t: String) extends Type
  case class ParamType(t: Type, params: Seq[Type]) extends Type

  def namedTypeName: Parser[Type] = (
    "[uU]8".r  ^^^ U8  |||
    "[uU]16".r ^^^ U16 |||
    "[uU]32".r ^^^ U32 |||
    "[uU]64".r ^^^ U64 |||
    "[sS]8".r  ^^^ S8  |||
    "[sS]16".r ^^^ S16 |||
    "[sS]32".r ^^^ S32 |||
    "[sS]64".r ^^^ S64 |||
    "[fF]32".r ^^^ F32 |||
    "[fF]64".r ^^^ F64 |||
    "_" ^^^ WildCard |||
    (identifier ^^ (NamedType(_)))
  )
  def typeName: Parser[Type] = (
    namedTypeName |||
    ("[" ~> repsep(typeParam ^^ (x => (x.name, x.t)) | typeName ^^ (("", _)), ",") <~ "]" ^^ (x => TupleType(x.zipWithIndex.map {
      case (("", t), i) => (s"_$i", t)
      case (s, _) => s
    }))) |||
    (namedTypeName ~ typeParams ^^ {case a~b => ParamType(a,b)})
  )

  sealed trait Expression extends Positional
  case class Block(exprs: Seq[Expression]) extends Expression
  case class Ident(name: String) extends Expression
  case class MemAccess(on: Expression, member: String) extends Expression
  case class ByName(name: String) extends Expression

  def byNameFunction = "'" ~> identifier ^^ ByName.apply

  def nonRecursiveExpression: Parser[Expression] =
    literal |||
    block |||
    (identifier ^^ (Ident(_))) |||
    byNameFunction |||
    structOrTuple |||
    (funCases)

  def nonCallExpression: Parser[Expression] = positioned (
    literal |||
    block |||
    (identifier ^^ (Ident(_))) |||
    byNameFunction |||
    structOrTuple |||
    (funCases) |||
    memAccess
  )

  def memAccess = chainl1(nonRecursiveExpression, identifier, "." ^^^ ({
    (a: Expression,  b: String) => MemAccess(a, b)
  }))
  
  def expression: Parser[Expression] = positioned (
    memAccess |||
    literal |||
    block |||
    (identifier ^^ (Ident(_))) |||
    funCall |||
    (funCases) |||
    byNameFunction |||
    structOrTuple |||
    fun
  )

  case class Struct(membs: Seq[Assignment]) extends Expression
  def structOrTuple =
    "[" ~> repsep(nonCallExpression ||| assignment, ",") <~ "]" ^^ (_.zipWithIndex.map {
      case (a: Assignment, _) => a
      case (x, i) => Assignment(s"_$i", x)
    }) ^^ Struct.apply

  def endl = accept(';') | accept('\n')

  def block = positioned("(" ~> repsep(expression, endl) <~ ")" ^^ (Block(_)))

  def identifier =
    "[a-zA-Z_][a-zA-Z_0-9]*".r |
    "[!$a-zA-Z_][!$a-zA-Z_0-9]*".r ^^ {x => Console.err.println(s"Warning: reserved character used in identifier $x"); x}

  case class Multiple(t: Type) extends Type

  def typeList: Parser[Seq[Type]] =
    (typeName <~ ",") ~ typeList ^^ (a => a._1 +: a._2) |||
    (typeName <~ "*" ^^ (Multiple(_)) ^^ (Seq(_))) |||
    typeName ^^ (Seq(_))

  def typeParams = "[" ~> typeList <~ "]"
  case class AnonFun(cases: Seq[FunCase]) extends Expression
  case class FunCase(params: Seq[Param], expr: Expression)
  def funCases = (
    (("|" ~> repsep(positioned(param), ",") ~ expression ^^ {case a~b => FunCase(a,b)}))
  ).+ ^^ (AnonFun(_))
  case class Assignment(name: String, expr: Expression) extends Expression
  case class ExtFun(name: String, signature: Seq[Type], returns: Option[Type]) extends Expression
  def extFun = 
    identifier ~ ("/" ~> typeParams ~ ("/" ~> typeName).?) ^^ (x => ExtFun(x._1, x._2._1, x._2._2))
  def assignment = 
    identifier ~ (":" ~> expression) ^^ (x => Assignment(x._1, x._2))
  def fun: Parser[Expression] =
    positioned(assignment | extFun)

  case class FunCall(fun: Expression, params: Seq[Expression]) extends Expression
  def __funcallpf(a: Expression, b: Expression) = a match {
    case a: FunCall => FunCall(a.fun, a.params :+ b)
    case a => FunCall(a, Seq(b))
  }
  def funCall =
    chainl1((identifier ^^ (Ident(_))), positioned(nonCallExpression), not(accept('\n')) ^^^ (__funcallpf(_,_)))
  //identifier ~ (repsep(expression, not(accept('\n')))) ^^ {case a~b => FunCall(a,b)}

  sealed trait Param extends Positional
  def param = "_" ^^^ WildCard | typeParam | literal

  case class TypeParam(name: String, t: Type) extends Param
  def typeParam: Parser[TypeParam] = identifier ~ ("/" ~> typeName) ^^ (x => TypeParam(x._1, x._2))

  case class IntLiteral(value: BigInt) extends Literal
  def intLiteral = "[0-9]+".r ^^ (BigInt(_)) ^^ (IntLiteral(_))
  case class FloatLiteral(value: BigDecimal) extends Literal
  def floatLiteral = "[0-9]+.[0-9]+".r ^^ (BigDecimal(_)) ^^ (FloatLiteral(_))
  case class StringLiteral(value: String) extends Literal
  def stringLiteral = "[\"]".r ~> "[^\"]+".r <~ "[\"]".r ^^ (StringLiteral(_))

  def literal = intLiteral | floatLiteral | stringLiteral
}