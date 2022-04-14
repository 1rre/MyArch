package es.tmoor.dynarch
import es.tmoor.dynarch.Parser._

object Assembler {
  final val IdMax = (1 << 10) - 1
  implicit class BinaryParser(val sc: StringContext) extends AnyVal {
    def b2(args: Any*): Byte = BigInt(sc.parts.mkString, 2).toByte
    def s2(args: Any*): Short = BigInt(sc.parts.mkString, 2).toShort
    def i2(args: Any*): Int = BigInt(sc.parts.mkString, 2).toInt
    def l2(args: Any*): Long = BigInt(sc.parts.mkString, 2).toLong
  }
  object b2 {
    def unapply(s: Int): Option[String] =
      Some(s.toBinaryString.reverse.padTo(8, '0').reverse)
  }
}
class Assembler {
  import Assembler._

  def literal(l: Long): List[Byte] = {
    val parts = l.toBinaryString.grouped(8).map(b => StringContext(b).b2()).toList.padTo(8, 0.toByte)
    val leading = parts.reverse.takeWhile(x => x == 0 || x == -1)
    if (leading.length <= 3)      b2"00000011" :: parts.take(8) // 64 bit
    else if (leading.length <= 5) b2"00000001" :: parts.take(4) // 32 bit
    else if (leading.length <= 7) b2"00000010" :: parts.take(2) // 16 bit
    else                          b2"00000000" :: parts.take(1) // 8  bit
  }

}