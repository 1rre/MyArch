package es.tmoor.dynarch
import Parser._
import collection.mutable.Buffer
import scala.reflect._
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import java.lang.Float.{floatToIntBits, intBitsToFloat}

object Interpreter {
  sealed abstract class RTValue[T](val t: Type)(implicit val num: Numeric[T]) {
    import num._
    val value: T
    def upgradeF32 = RF32(value.toFloat)
    def upgradeF64 = RF64(value.toDouble)
    def upgradeI64 = RI64(value.toLong)

    def asMemAddr: Int
    def div: (T,T) => T
    def rem: (T,T) => T
    
    type CastT = [T <: Type] =>> T match {
      case I8.type => RI8
      case I16.type => RI16
      case I32.type => RI32
      case I64.type => RI64
      case F32.type => RF32
      case F64.type => RF64
    }

    def asI64: Long
    def cast[T <: Type : ClassTag]: CastT[T] = 
      (classTag[T].runtimeClass match {
        case `i8_t` => RI8(asI64.toByte)
        case `i16_t` => RI16(asI64.toShort)
        case `i32_t` => RI32(asI64.toInt)
        case `i64_t` => RI64(asI64)
        case `f32_t` => RF32(intBitsToFloat(asI64.toInt))
        case `f64_t` => RF64(longBitsToDouble(asI64))
      }).asInstanceOf[CastT[T]]
  }
  type BaseType[T] = (RF64 & RTValue[T] | RF32 & RTValue[T] | RI64 & RTValue[T])
  def arithmeticOp[T](pair: (BaseType[T], BaseType[T]), op: (T, T) => T) =
    pair._1.getClass.getConstructors.head.newInstance(op(pair._1.value, pair._2.value)).asInstanceOf[BaseType[T]]
    
  def booleanOp[T](pair: (BaseType[T], BaseType[T]), op: (T, T) => Boolean): RI64 =
    if (op(pair._1.value, pair._2.value)) RI64(1) else RI64(0)

  def mul[T](pair: (BaseType[T], BaseType[T])): BaseType[T] = arithmeticOp(pair, pair._1.num.times)
  def add[T](pair: (BaseType[T], BaseType[T])): BaseType[T] = arithmeticOp(pair, pair._1.num.plus)
  def sub[T](pair: (BaseType[T], BaseType[T])): BaseType[T] = arithmeticOp(pair, pair._1.num.minus)
  def div[T](pair: (BaseType[T], BaseType[T])): BaseType[T] = arithmeticOp(pair, pair._1.div)


  def rem[T](pair: (BaseType[T], BaseType[T])): BaseType[T] = arithmeticOp(pair, pair._1.rem)
  def eq_[T](pair: (BaseType[T], BaseType[T])): RI64 = booleanOp(pair, pair._1.num.equiv)
  def ne_[T](pair: (BaseType[T], BaseType[T])): RI64 = booleanOp(pair, pair._1.num.equiv)
  def ge[T](pair: (BaseType[T], BaseType[T])): RI64 = booleanOp(pair, pair._1.num.gteq)
  def lt[T](pair: (BaseType[T], BaseType[T])): RI64 = booleanOp(pair, pair._1.num.gteq)  

  sealed abstract class RTInt[T](t: Type)(implicit num: Integral[T]) extends RTValue[T](t: Type) {
    def asMemAddr: Int = upgradeI64.value.toInt
    def div = num.quot
    def rem = num.rem
    import num._
  }

  private val i8_t = classOf[I8.type]
  private val i16_t = classOf[I16.type]
  private val i32_t = classOf[I32.type]
  private val i64_t = classOf[I64.type]
  private val f32_t = classOf[F32.type]
  private val f64_t = classOf[F64.type]
  sealed abstract class RTFloat[T](t: Type)(implicit num: Fractional[T]) extends RTValue[T](t: Type) {
    def div = num.div
  }
  case class RI8(value: Byte) extends RTInt[Byte](I8) {
    def asI64 = (value | 0L)
  }
  case class RI16(value: Short) extends RTInt[Short](I16) {
    def asI64 = (value | 0L)
  }
  case class RI32(value: Int) extends RTInt[Int](I32) {
    def asI64 = (value | 0L)
  }
  case class RI64(value: Long) extends RTInt[Long](I64) {
    def asI64 = (value | 0L)
  }
  case class RF32(value: Float) extends RTFloat[Float](F32) {
    
    def rem = (a,b) => a%b
    def asMemAddr: Int = {
      println(s"Attempt to dereference float: $this - casting to int")
      floatToIntBits(value)
    }
    def asI64 = floatToIntBits(value) | 0L
  }
  case class RF64(value: Double) extends RTFloat[Double](F64) {
    
    def rem = (a,b) => a%b
    def asMemAddr: Int = {
      println(s"Attempt to dereference double: $this - casting to int")
      doubleToLongBits(value).toInt
    }
    def asI64 = doubleToLongBits(value)
  }
}

class Interpreter {
  import Interpreter._

  var acc: RTValue[?] = RI64(0l)
  val reg: Array[RTValue[?]] = Array.fill(256)(RI64(0l))
  val mem: Buffer[Byte] = Buffer()
  var sp = 0

  def getMem(at: Int, t: Type): RTValue[?] = t match {
    case Keep =>
      getMem(at, acc.t)
    case I32 =>
    RI32(mem.slice(at, at + 5).map(_ & 0xFF).reduceRight((v,acc) => (acc << 8) + v))
    case I64 =>
      RI64(mem.slice(at, at + 9).map(_ & 0xFFL).reduceRight((v,acc) => (acc << 8) + v))
    case F32 =>
      RF32(intBitsToFloat(mem.slice(at, at + 5).map(_ & 0xFF).reduceRight((v,acc) => (acc << 8) + v)))
    case F64 => 
      RF64(longBitsToDouble(mem.slice(at, at + 9).map(_ & 0xFFL).reduceRight((v,acc) => (acc << 8) + v)))
    case _ =>
      sys.error(s"Get mem of type $t Not Implemented")
  }

  def setMem(at: Int, t: Type): Unit = {
    while (mem.size < at + 8) mem += 0.toByte
    t match {
      case Keep =>
        println(s"Keeping T")
        setMem(at, acc.t)
      case I8 =>
        val data = acc.cast[I8.type].value
        mem(at) = data.toByte
      case I16 =>
        val data = acc.cast[I16.type].value
        mem(at) = data.toByte
        mem(at + 1) = (data >>> 8).toByte
      case I32 | F32 =>
        val data = acc.cast[I32.type].value
        mem(at) = data.toByte
        mem(at + 1) = (data >>> 8).toByte
        mem(at + 2) = (data >>> 16).toByte
        mem(at + 3) = (data >>> 24).toByte
      case I64 | F64 => 
        val data = acc.cast[I64.type].value
        mem(at) = data.toByte
        mem(at + 1) = (data >>> 8).toByte
        mem(at + 2) = (data >>> 16).toByte
        mem(at + 3) = (data >>> 24).toByte
        mem(at + 4) = (data >>> 32).toByte
        mem(at + 5) = (data >>> 40).toByte
        mem(at + 6) = (data >>> 48).toByte
        mem(at + 7) = (data >>> 56).toByte
    }
    println(s"After set at $at of T: $t")
    println(mem)
  }

  def runtime(v: Value): RTValue[?] = v match {
    case FloatLiteral(value) => RF64(value)
    case IntLiteral(value) => RI64(value)
    case Register(id) => reg(id.toInt)
    case Reference(to, t) =>
      val v = reg(to)
      if (!v.isInstanceOf[RTInt[?]]) println(s"Reg $to is not Int. Casting to Int.")
      getMem(v.asMemAddr, t)
    case Stack(at, t) =>
      getMem(sp + at, t)
  }

  /*
  def upgrade[Upgrader[T]] = (v1: RTValue[T1], v2: RTValue[T2]) => (v1, v2) match {
      case (_: RF64, _) | (_, _: RF64) => (v1.upgradeF64, v2.upgradeF64)
      case (_: RF32, _) | (_, _: RF32) => (v1.upgradeF32[ArithmeticType[TR]], v2.upgradeF32[ArithmeticType[TR]])
      case (_, _) => (v1.upgradeI64[ArithmeticType[TR]], v2.upgradeI64[ArithmeticType[TR]])
  }
  */

  def upgrade[T <: (RI64, RI64) | (RF64, RF64) | (RF32, RF32)](v1: RTValue[?], v2: RTValue[?]): T = {
    (v1.t, v2.t) match {
      case (F64, _) | (_, F64) => (v1.upgradeF64, v2.upgradeF64).asInstanceOf[T]
      case (F32, _) | (_, F32) => (v1.upgradeF32, v2.upgradeF32).asInstanceOf[T]
      case (_, _) => (v1.upgradeI64, v2.upgradeI64).asInstanceOf[T]
    }
  }
  def interpret(prog: Seq[Operation]): Unit =
    if (!prog.isEmpty) {
      println(s"Acc: $acc")
      //println(s"Mem: $mem")
      //println(s"Reg: ${reg.toSeq}")
      println(prog.head)
      type T1
      type T2 = (BaseType[T1], BaseType[T1]) & ((RI64, RI64) | (RF64, RF64) | (RF32, RF32))
      prog.head match {
        case Load(v) => acc = runtime(v)
        case Mul(v) => acc = mul(upgrade[T2](acc, runtime(v)))
        case Add(v) => acc = add(upgrade[T2](acc, runtime(v)))
        case Sub(v) => acc = sub(upgrade[T2](acc, runtime(v)))
        case Div(v) => acc = div(upgrade[T2](acc, runtime(v)))
        case Rem(v) => acc = rem(upgrade[T2](acc, runtime(v)))
        case Eq(v) => acc = eq_(upgrade[T2](acc, runtime(v)))
        case Ne(v) => acc = ne_(upgrade[T2](acc, runtime(v)))
        case Ge(v) => acc = ge(upgrade[T2](acc, runtime(v)))
        case Lt(v) => acc = lt(upgrade[T2](acc, runtime(v)))
        case Jump(to) => sys.error(s"Jump to $to unimplemented")
        case BNez(to) => sys.error(s"BNez to $to unimplemented")
        case And(v) => sys.error(s"And $v unimplemented")
        case Or(v) => sys.error(s"Or $v unimplemented")
        case Lsl(v) => sys.error(s"Lsl by $v unimplemented")
        case Put(Register(id)) =>
          reg(id) = acc
        case Put(Reference(id, t)) =>
          val v = reg(id)
          if (!v.isInstanceOf[RI32]) Console.err.println(s"Reg $id is not I32. Casting to I32.")
          setMem(v.asMemAddr, t)
        case Put(Stack(at, t)) =>
          setMem(sp + at, t)
      }
      interpret(prog.tail)
    }
}