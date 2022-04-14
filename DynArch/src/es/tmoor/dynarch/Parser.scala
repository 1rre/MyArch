package es.tmoor.dynarch

import scala.util.matching.Regex
import scala.reflect.ClassTag
import java.awt.Stroke

object Parser extends util.parsing.combinator.RegexParsers {
  override def skipWhitespace: Boolean = true

  sealed trait Type 
  case object Keep extends Type
  sealed abstract class SizedType(val size: Int, m: Parser[_]) extends Type {
    def p: Parser[SizedType] = m ~ size.toString ^^^ this
  }
  sealed abstract class FloatType(size: Int) extends SizedType(size, "[fF]".r)
  sealed abstract class IntType(size: Int) extends SizedType(size, "[iI]".r)
  
  sealed trait Value
  sealed trait Storage extends Value
  sealed trait Memory extends Storage {
    val t: Type
  }
  sealed trait Literal extends Value
  
  case object I8 extends IntType(8)
  case object I16 extends IntType(16)
  case object I32 extends IntType(32)
  case object I64 extends IntType(64)
  def intType = I8.p | I16.p | I32.p | I64.p

  case object F32 extends FloatType(32)
  case object F64 extends FloatType(64)
  def floatType = F32.p | F64.p
  
  def typeSpecifier = intType | floatType

  case class FloatLiteral(value: Double) extends Literal
  case class IntLiteral(value: Long) extends Literal
  
  def intLiteral = "(-?)[0-9]+".r ^^ (IntLiteral apply _.toLong)
  def floatLiteral = """(-?)[0-9]+\.[0-9]+""".r ^^ (FloatLiteral apply _.toDouble)
  def lit = floatLiteral | intLiteral

  case class Register(id: Byte) extends Storage
  def register = "x" ~> "[0-9]+".r ^^ (Register apply _.toInt.toByte)
  case class Stack(offset: Byte, t: Type) extends Memory
  def stack = (typeSpecifier <~ "@").? ~ ("y" ~> "[0-9]+".r) ^^ {
    case Some(t) ~ n => Stack(n.toInt.toByte, t)
    case None ~ n => Stack(n.toInt.toByte, Keep)
  }
  case class Reference(to: Byte, t: Type) extends Memory
  def reference = (typeSpecifier <~ "@").? ~ ("[" ~> register <~ "]") ^^ {
    case Some(t) ~ n => Reference(n.id, t)
    case None ~ n => Reference(n.id, Keep)
  }
  def mem = stack | reference

  def value = mem | register | lit

  sealed trait Operation
  sealed abstract class Op1(operator: String, fn: Value => Operation) {
    def apply(v: Value): Operation = fn(v)
    def p = operator ~> value ^^ apply
  }

  object LoadP {
    def p = value ^? {
      case m: Memory if m.t.isInstanceOf[SizedType] => Load(m)
      case v if !v.isInstanceOf[Memory] => Load(v)
    }
  }
  case class Load(v: Value) extends Operation
  case class Mul(v: Value) extends Operation
  object MulP extends Op1("*", Mul.apply)
  case class Add(v: Value) extends Operation
  object AddP extends Op1("+", Add.apply)
  case class Sub(v: Value) extends Operation
  object SubP extends Op1("-", Sub.apply)
  case class Div(v: Value) extends Operation
  object DivP extends Op1("/", Div.apply)
  case class Mod(v: Value) extends Operation
  object ModP extends Op1("%", Mod.apply)
  case class Eq(v: Value) extends Operation
  object EqP extends Op1("=", Eq.apply)
  case class Ne(v: Value) extends Operation
  object NeP extends Op1("~", Ne.apply)
  case class Ge(v: Value) extends Operation
  object GeP extends Op1(">", Ge.apply)
  case class Lt(v: Value) extends Operation
  object LtP extends Op1("<", Lt.apply)
  case class Jump(to: Value) extends Operation
  object JumpP extends Op1("#", Jump.apply)
  case class BNez(to: Value) extends Operation
  object BNezP extends Op1("?", BNez.apply)
  case class Put(to: Storage) extends Operation
  object PutP {
    def p = ":" ~> (mem | register) ^^ Put.apply
  }
  case class Lsl(v: Value) extends Operation
  object LslP extends Op1("'", Lsl.apply)
  case class And(v: Value) extends Operation
  object AndP extends Op1("&", And.apply)
  case class Or(v: Value) extends Operation
  object OrP extends Op1("|", Or.apply)

  def operation =
    LoadP.p | MulP.p | AddP.p | SubP.p | DivP.p | ModP.p | EqP.p | NeP.p |
    GeP.p | LtP.p | JumpP.p | BNezP.p | PutP.p | LslP.p | AndP.p | OrP.p
}

/*

  sealed trait Value
  sealed trait Operation
  sealed abstract class Operator[T: ClassTag] extends Token {
    def apply(v: Value) = {
      val c = reflect.classTag[T].runtimeClass
      val obj = c.getConstructor().newInstance()
      c.getMethod("apply", classOf[Value])
        .invoke(obj, v)
        .asInstanceOf[Operation]
    }
  }
  case object Equal extends Operator[Eq.type]
  case object NotEqual extends Operator[Ne.type]
  case object GreaterEqual extends Operator[Ge.type]
  case object LessEqual extends Operator[Le.type]
  case object Greater extends Operator[Gt.type]
  case object Less extends Operator[Lt.type]
  case object Add extends Operator[Ad.type]
  case object Sub extends Operator[Sb.type]
  case object Mul extends Operator[Ml.type]
  case object Div extends Operator[Dv.type]
  case object Mod extends Operator[Md.type]
  def operator = Seq (
    ">=" ^^^ GreaterEqual, "=<" ^^^ LessEqual, ">" ^^^ Greater, "<" ^^^ Less,
    "/=" ^^^ NotEqual, "=" ^^^ Equal,
    "+" ^^^ Add, "-" ^^^ Sub, "*" ^^^ Mul, "/" ^^^ Div, "%" ^^^ Mod
  ).reduce(_|_)

  case class Text(t: String) extends Operator
  case class FloatLiteral(t: String) extends Operator with Value
  case class IntLiteral(t: String) extends Operator with Value
  def text = "[a-z]+".r ^^ Text.apply
  def float = """(-?)[0-9]+\.[0-9]+""".r ^^ FloatLiteral.apply
  def int = "(-?)[0-9]+".r ^^ IntLiteral.apply

  sealed trait Storage extends Value
  case class Memory(offset: Int) extends Storage
  case class Register(id: Int) extends Storage
  case class RefTo(id: Int) extends Storage

  def kw(name: String) = text ^? {case Text(`name`) => name}

  def memory = kw("y") ~> int ^^ (Memory apply _.t.toInt)
  def register = kw("x") ~> int ^^ (Register apply _.t.toInt)
  def refTo = "[" ~> (register) <~ "]" ^^ (_.id) ^^ RefTo.apply

  sealed trait Type
  case object F32 extends Type
  case object F64 extends Type
  case object I8 extends Type
  case object I16 extends Type
  case object I32 extends Type
  case object I64 extends Type
  def floatT = kw("f") ~> int ^? {
    case IntLiteral("32") => F32
    case IntLiteral("64") => F64
  }
  def intT = kw("i") ~> int ^? {
    case IntLiteral("8") => I8
    case IntLiteral("16") => I16
    case IntLiteral("32") => I32
    case IntLiteral("64") => I64
  }
  case class Eq(value: Value) extends Operation
  case class Ne(value: Value) extends Operation
  case class Ad(value: Value) extends Operation
  case class Sb(value: Value) extends Operation
  case class Ml(value: Value) extends Operation
  case class Dv(value: Value) extends Operation
  case class Md(value: Value) extends Operation
  case class Gt(value: Value) extends Operation
  case class Lt(value: Value) extends Operation
  case class Le(value: Value) extends Operation
  case class Ge(value: Value) extends Operation
  case class As(to: Storage) extends Operation
  case class Ld(value: Value) extends Operation
  def basicValue: Parser[Value] = float | int | register | memory | refTo
  def operation = operator ~ value ^^ {case a~b => a(b)}
  def typeConversion = (("//" ~> (floatT | intT)) ^^ TypeConversion.apply) | ("""\\""" ~> (floatT | intT) ^^ TypeCast.apply)
  def assign = "->" ~> value ^? {
    case to: Storage => As(to)
  }
  def value = basicValue
  def statement: Parser[Operation] = assign | operation | typeConversion | value ^^ Ld.apply
*/