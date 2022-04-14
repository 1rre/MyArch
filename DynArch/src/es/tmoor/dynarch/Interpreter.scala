package es.tmoor.dynarch
import Parser._
import collection.mutable.Buffer
import scala.reflect._
import java.lang.Double.{doubleToLongBits, longBitsToDouble}
import java.lang.Float.{floatToIntBits, intBitsToFloat}
class Interpreter {
  sealed abstract class RTValue[T](val t: Type) {
    val value: T
    def asI32: Int
    def cast[T <: Type: ClassTag]: RTValue[_]
  }

  var acc: RTValue[_] = RI64(0l)
  val reg: Array[RTValue[_]] = Array.fill(256)(RI64(0l))
  val mem: Buffer[Byte] = Buffer()
  var sp = 0

  sealed abstract class RTInt[T](t: Type) extends RTValue[T](t: Type)
  sealed abstract class RTFloat[T](t: Type) extends RTValue[T](t: Type)
  
  private val i32_t = classOf[I32.type]
  private val i64_t = classOf[I64.type]
  private val f32_t = classOf[F32.type]
  private val f64_t = classOf[F64.type]
  case class RI32(value: Int) extends RTInt[Int](I32) {
    def asI32: Int = value
    def cast[T <: Type: ClassTag]: RTValue[_] = {
      classTag[T].runtimeClass match {
        case `i32_t` => this
        case `i64_t` => RI64(value & 0xFFFFFFFFL)
        case `f32_t` => RF32(intBitsToFloat(value))
        case `f64_t` => RF64(longBitsToDouble(value & 0xFFFFFFFFL))
      }
    }
  }
  case class RI64(value: Long) extends RTInt[Long](I64) {
    def asI32: Int = (value & 0xFFFFFFFF).toInt
    def cast[T <: Type: ClassTag]: RTValue[_] = {
      classTag[T].runtimeClass match {
        case `i32_t` => RI32((value & 0xFFFFFFFFL).toInt)
        case `i64_t` => this
        case `f32_t` => RF32(intBitsToFloat((value & 0xFFFFFFFFL).toInt))
        case `f64_t` => RF64(longBitsToDouble(value))
      }
    }
  }
  case class RF32(value: Float) extends RTFloat[Float](F32) {
    def asI32: Int = floatToIntBits(value)
    def cast[T <: Type: ClassTag]: RTValue[_] = {
      classTag[T].runtimeClass match {
        case `i32_t` => RI32(asI32)
        case `i64_t` => RI64(asI32 & 0xFFFFFFFFL)
        case `f32_t` => this
        case `f64_t` => RF64(longBitsToDouble(asI32 & 0xFFFFFFFFL))
      }
    }
  }
  case class RF64(value: Double) extends RTFloat[Double](F64) {
    def asI32: Int = (doubleToLongBits(value) & 0xFFFFFFFF).toInt
    def cast[T <: Type: ClassTag]: RTValue[_] = {
      classTag[T].runtimeClass match {
        case `i32_t` => RI32(asI32)
        case `i64_t` =>
          println(s"Casting from $value to ${doubleToLongBits(value)}")
          RI64(doubleToLongBits(value))
        case `f32_t` => RF32(asI32)
        case `f64_t` => this
      }
    }
  }

  def getMem(at: Int, t: Type): RTValue[_] = t match {
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
        setMem(at, acc.t)
      case I32 =>
        if (!(acc.t == I32)) println(s"Acc (${acc.t}) is not I32. Casting to I32.")
        val data = acc.cast[I32.type].asInstanceOf[RI32].value
        mem(at) = (data & 0xFF).toByte
        mem(at + 1) = ((data >> 8) & 0xFF).toByte
        mem(at + 2) = ((data >> 16) & 0xFF).toByte
        mem(at + 3) = ((data >> 24) & 0xFF).toByte
      case I64 => 
        if (!(acc.t == I64)) println(s"Acc (${acc.t}) is not I64. Casting to I64.")
        val data = acc.cast[I64.type].asInstanceOf[RI64].value
        mem(at) = (data & 0xFF).toByte
        mem(at + 1) = ((data >> 8) & 0xFF).toByte
        mem(at + 2) = ((data >> 16) & 0xFF).toByte
        mem(at + 3) = ((data >> 24) & 0xFF).toByte
        mem(at + 4) = ((data >> 32) & 0xFF).toByte
        mem(at + 5) = ((data >> 40) & 0xFF).toByte
        mem(at + 6) = ((data >> 48) & 0xFF).toByte
        mem(at + 7) = ((data >> 56) & 0xFF).toByte
      case F32 =>
        acc = acc.cast[I32.type]
        setMem(at, I32)
        acc = acc.cast[F32.type]
      case F64 =>
        acc = acc.cast[I64.type]
        setMem(at, I64)
        acc = acc.cast[F64.type]
    }
    println(mem)
  }

  def runtime(v: Value): RTValue[_] = v match {
    case FloatLiteral(value) => RF64(value)
    case IntLiteral(value) => RI64(value)
    case Register(id) => reg(id.toInt)
    case Reference(to, t) =>
      val v = reg(to)
      if (!v.isInstanceOf[RTInt[_]]) println(s"Reg $to is not Int. Casting to Int.")
      getMem(v.asI32, t)
    case Stack(at, t) =>
      getMem(sp + at, t)
  }

  def interpret(prog: Seq[Operation]): Unit =
    if (!prog.isEmpty) {
      println(s"Acc: $acc")
      //println(s"Mem: $mem")
      //println(s"Reg: ${reg.toSeq}")
      println(prog.head)
      prog.head match {
        case Load(v) => acc = runtime(v)
        case Put(Register(id)) =>
          reg(id) = acc
        case Put(Reference(id, t)) =>
          val v = reg(id)
          if (!v.isInstanceOf[RI32]) Console.err.println(s"Reg $id is not I32. Casting to I32.")
          setMem(v.asI32, t)
        case Put(Stack(at, t)) =>
          setMem(sp + at, t)
      }
      interpret(prog.tail)
    }
}