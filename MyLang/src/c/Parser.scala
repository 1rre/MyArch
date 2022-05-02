package c

import util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  def digit = "[0-9]".r
  def nondigit = "[_a-zA-Z]".r
  def idchar = digit | nondigit
  def identifier = nondigit ~ idchar.* ^^ (x => (x._1 + x._2.mkString))
  
  sealed trait Expression
  sealed trait Constant extends Expression
  def constant = floatingConstant | integerConstant | enumConstant | charConstant
  
  case class FloatingConstant(fp: FractionalConstant, exp: Option[ExponentPart], suffix: Option[String]) extends Constant
  def floatingConstant = 
    (fractionalConstant ~ exponentPart.? ~ floatingSuffix.? ^^ (x =>
      FloatingConstant(x._1._1, x._1._2, x._2))
    ) |
    (digitSeq ~ exponentPart ~ floatingSuffix.? ^^ (x =>
      FloatingConstant(FractionalConstant(x._1._1, "0"), Some(x._1._2), x._2))
    )

  case class FractionalConstant(a: String, b: String)
  def fractionalConstant =
    (digitSeq.? ~ "." ~ digitSeq ^^ {case a~_~b =>
      FractionalConstant(a.getOrElse("0"), b)}
    ) |
    (digitSeq <~ "." ^^ (x => FractionalConstant(x, "0")))

  case class ExponentPart(sign: String, ds: String)
  def exponentPart = "[eE]".r ~> sign.? ~ digitSeq ^^ (x =>
    ExponentPart(x._1.getOrElse("+"), x._2)
  )

  def digitSeq = digit.+ ^^ (_.mkString)
  def sign = "[+-]".r

  def floatingSuffix = "flFL".r

  case class IntegerConstant(value: BigInt, suffix: Option[String]) extends Constant
  def integerConstant = 
    (decimalConstant | octalConstant | hexConstant) ~ intSuffix.? ^^ (x => IntegerConstant(x._1, x._2))
  def decimalConstant = "[1-9]".r ~ digit.+ ^^ (x => BigInt((x._1 +: x._2).mkString))
  def octalConstant = "0[0-7]+".r ^^ (BigInt(_, 8))
  def hexConstant = "0x[0-9a-fA-F]+".r ^^ (_.drop(2)) ^^ (BigInt(_, 16))
  def intSuffix = "[uU]?[lL]?".r | "[lL]?[uU]?".r

  // Note: Potential shift/reduce conflict?
  case class EnumConstant(name: String) extends Constant
  def enumConstant = identifier ^^ EnumConstant.apply

  def charConstant = "L".? ~ "'" ~> cChar.+ <~ "'" ^^ (x => IntegerConstant(x.foldLeft(BigInt(0))((acc, v) => (acc << 8) + v), None))
  def cChar = escape ||| ("""[^\\'\n]""".r ^^ (_.head.toInt))
  def escape = """\\['"?\\abfnrtv]""".r ^^^ (sys.error("escapes not implemented"))

  def stringLiteral = "L?\"".r ~> sChar.+ <~ "\"" ^^ (_.mkString)
  def sChar = escape ||| ("""[^\\"\n]""".r)

  case class IdExpression(id: String) extends Expression
  case class StringExpression(str: String) extends Expression
  def primaryExpression = 
    identifier ^^ IdExpression.apply |
    constant |
    stringLiteral ^^ StringExpression.apply |
    "(" ~> expression <~ ")"

  case class AccessPostfix(lhs: Expression, rhs: Expression) extends Expression
  case class FnCallPostfix(lhs: Expression, rhs: Seq[Expression]) extends Expression
  case class MemberPostfix(lhs: Expression, rhs: String) extends Expression
  case class PtrMemberPostfix(lhs: Expression, rhs: String) extends Expression
  case class Increment(value: Expression) extends Expression
  case class Decrement(value: Expression) extends Expression
  def postfixTail: Parser[Expression => Expression] =
    ("[" ~> expression <~ "]") ^^ (x => y => AccessPostfix(y, x)) |
    ("(" ~> repsep(assignmentExpression, ",") <~ ")") ^^ (x => y => FnCallPostfix(y, x)) |
    ("." ~> identifier) ^^ (x => y => MemberPostfix(y, x)) |
    ("->" ~> identifier) ^^ (x => y => PtrMemberPostfix(y, x)) |
    ("++" ^^^ (Increment(_))) |
    ("--" ^^^ (Decrement(_)))
    
  def postfixExpression =
    primaryExpression ~ postfixTail.* ^^ {case a~b => b.foldLeft(a)((acc, v) => v(acc))}

  def unaryExpression =
    postfixExpression
  
  def castExpression =
    unaryExpression

  case class MultiplyExpression(lhs: Expression, rhs: Expression) extends Expression
  case class DivideExpression(lhs: Expression, rhs: Expression) extends Expression
  case class ModuloExpression(lhs: Expression, rhs: Expression) extends Expression

  def multiExpression =
    chainl1[Expression](castExpression,
      ("*" ^^^ ((a: Expression, b: Expression) => MultiplyExpression(a, b))) |
      ("/" ^^^ ((a: Expression, b: Expression) => DivideExpression(a, b))) |
      ("%" ^^^ ((a: Expression, b: Expression) => ModuloExpression(a, b)))
    )

  case class AddExpression(lhs: Expression, rhs: Expression) extends Expression
  case class SubExpression(lhs: Expression, rhs: Expression) extends Expression
  def addExpression =
    chainl1[Expression](multiExpression,
      ("+" ^^^ ((a: Expression, b: Expression) => AddExpression(a, b))) |
      ("-" ^^^ ((a: Expression, b: Expression) => SubExpression(a, b)))
    )

  case class ShiftLeftExpression(lhs: Expression, rhs: Expression) extends Expression
  case class ShiftRightExpression(lhs: Expression, rhs: Expression) extends Expression
  def shiftExpression =
    chainl1[Expression](multiExpression,
      ("<<" ^^^ ((a: Expression, b: Expression) => ShiftLeftExpression(a, b))) |
      (">>" ^^^ ((a: Expression, b: Expression) => ShiftRightExpression(a, b)))
    )

  case class LtExpression(lhs: Expression, rhs: Expression) extends Expression
  case class GtExpression(lhs: Expression, rhs: Expression) extends Expression
  case class LeExpression(lhs: Expression, rhs: Expression) extends Expression
  case class GeExpression(lhs: Expression, rhs: Expression) extends Expression
  def relationalExpression =
    chainl1[Expression](shiftExpression,
      ("<" ^^^ ((a: Expression, b: Expression) => LtExpression(a, b))) |
      (">" ^^^ ((a: Expression, b: Expression) => GtExpression(a, b))) |
      ("<=" ^^^ ((a: Expression, b: Expression) => LeExpression(a, b))) |
      (">=" ^^^ ((a: Expression, b: Expression) => GeExpression(a, b)))
    )
  
  case class EqExpression(lhs: Expression, rhs: Expression) extends Expression
  case class NeExpression(lhs: Expression, rhs: Expression) extends Expression
  def equalityExpression =
    chainl1[Expression](relationalExpression,
      ("==" ^^^ ((a: Expression, b: Expression) => EqExpression(a, b))) |
      ("!=" ^^^ ((a: Expression, b: Expression) => NeExpression(a, b)))
    )

  case class AndExpression(lhs: Expression, rhs: Expression) extends Expression
  def andExpression =
    chainl1[Expression](equalityExpression,
      ("&" ^^^ ((a: Expression, b: Expression) => AndExpression(a, b)))
    )
    
  case class XorExpression(lhs: Expression, rhs: Expression) extends Expression
  def xorExpression =
    chainl1[Expression](andExpression,
      ("^" ^^^ ((a: Expression, b: Expression) => XorExpression(a, b)))
    )

  case class OrExpression(lhs: Expression, rhs: Expression) extends Expression
  def orExpression =
    chainl1[Expression](xorExpression,
      ("|" ^^^ ((a: Expression, b: Expression) => OrExpression(a, b)))
    )
  
  case class LAndExpression(lhs: Expression, rhs: Expression) extends Expression
  def lAndExpression =
    chainl1[Expression](orExpression,
      ("&&" ^^^ ((a: Expression, b: Expression) => LAndExpression(a, b)))
    )
  
  case class LOrExpression(lhs: Expression, rhs: Expression) extends Expression
  def lOrExpression =
    chainl1[Expression](orExpression,
      ("||" ^^^ ((a: Expression, b: Expression) => LOrExpression(a, b)))
    )
  
  case class CondExpression(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
  def condExpression: Parser[Expression] =
    lOrExpression ~ ("?" ~> expression ~ (":" ~> condExpression)).? ^^ {
      case a~Some(b~c) => CondExpression(a, b, c)
      case a~None => a
    }
  
  case class Assignment(lhs: Expression, rhs: Expression) extends Expression
  case class MulAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class DivAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class ModAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class AddAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class SubAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class LslAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class LsrAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class AndAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class XorAssignment(lhs: Expression, rhs: Expression) extends Expression
  case class OrAssignment(lhs: Expression, rhs: Expression) extends Expression
  def assignmentOp =
    "=" ^^^ (Assignment(_, _)) |
    "*=" ^^^ (MulAssignment(_, _)) |
    "/=" ^^^ (DivAssignment(_, _)) |
    "%=" ^^^ (ModAssignment(_, _)) |
    "+=" ^^^ (AddAssignment(_, _)) |
    "-=" ^^^ (SubAssignment(_, _)) |
    "<<=" ^^^ (LslAssignment(_, _)) |
    ">>=" ^^^ (LsrAssignment(_, _)) |
    "&=" ^^^ (AndAssignment(_, _)) |
    "^=" ^^^ (XorAssignment(_, _)) |
    "|=" ^^^ (OrAssignment(_, _))
  def assignmentExpression: Parser[Expression] =
    (unaryExpression ~ assignmentOp ~ assignmentExpression ^^ {case a~b~c => b(a,c)}) |
    condExpression

  case class CommaExpression(lhs: Expression, rhs: Expression) extends Expression
  def expression =
    (chainl1(assignmentExpression, "," ^^^ (CommaExpression(_,_))))
  
  def constantExpression = condExpression


}