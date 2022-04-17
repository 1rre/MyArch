import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import es.tmoor.dynarch.hardware.Const._
import es.tmoor.dynarch.hardware.comb

class UnsignedALUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Unsigned ALU"

  final val NInputs = 10

  def testUnsigned(in1: Long, in2: Long, opcode: UInt, expected: Long) = (alu: comb.ALU) => {
    alu.io.in1.d.poke(in1.S(63, 0))
    alu.io.in2.d.poke(in2.S(63, 0))
    alu.io.in1.t.poke(TUnsigned)
    alu.io.in2.t.poke(TUnsigned)
    alu.io.op.poke(opcode)
    alu.clock.step()
    alu.io.out.t.expect(TUnsigned)
    alu.io.out.d.expect(expected.S(63, 0))
  }
  
  implicit class ExpTOps(x: Long) {
    def toExp = BigInt(0.toByte +: BigInt(x).toByteArray)
    def #+(that: ExpTOps) = (this.toExp + that.toExp).toLong
    def #-(that: ExpTOps) = (this.toExp - that.toExp).toLong
    def #*(that: ExpTOps) = (this.toExp * that.toExp).toLong
    def #%(that: ExpTOps) = (this.toExp % that.toExp).toLong
    def #/(that: ExpTOps) = (this.toExp / that.toExp).toLong
    def #==(that: ExpTOps) = if (this.toExp == that.toExp) 1L else 0L
    def #!=(that: ExpTOps) = if (this.toExp != that.toExp) 1L else 0L
    def #>=(that: ExpTOps) = if (this.toExp >= that.toExp) 1L else 0L
    def #<(that: ExpTOps) = if (this.toExp < that.toExp) 1L else 0L
    def #<<(that: Long) = (if (that >= 0) this.toExp << that.toInt else this.toExp >> (-that).toInt).toLong
    def #|(that: ExpTOps) = (this.toExp | that.toExp).toLong
    def #&(that: ExpTOps) = (this.toExp & that.toExp).toLong
  }
  
  
  it should "Multiply UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Mul, (x #* y)))
  }
  it should "Add UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Add, (x #+ y)))
  }
  
  it should "Subtract UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Sub, (x #- y)))
  }
  
  it should "Divide UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) {
      test(new comb.ALU)(testUnsigned(x, y, Div, x #/ y))
    }
  }
  
  it should "Remainder UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) {
      test(new comb.ALU)(testUnsigned(x, y, Rem, (x #% y)))
    }
  }
  
  it should "Equals UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Eq, x #== y))
  }

  it should "Not Equals UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Ne, x #!= y))
  }
  
  it should "Greater or Equal UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Ge, x #>= y))
  }

  it should "Less Than UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Lt, x #< y))
  }

  it should "Logic Shift UInts" in {
    val inputs = Seq.fill(NInputs)(Random.nextLong(), Random.between(-128L, 127L))
    for ((x,y) <- inputs) {
      test(new comb.ALU)(testUnsigned(x, y, Lsl, x #<< y))
    }
  }

  it should "And UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, And, x #& y))
  }

  it should "Or UInts" in {
    val inputs = Seq.fill(NInputs)((Random.nextLong(), Random.nextLong()))
    for ((x,y) <- inputs) test(new comb.ALU)(testUnsigned(x, y, Or, x #| y))
  }
}