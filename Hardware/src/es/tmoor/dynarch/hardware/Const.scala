package es.tmoor.dynarch.hardware
import chisel3._

object Const {
  final val ResetVector = 0x40000000.U

  final val Load = "b0000".U(4.W)
  final val Mul  = "b0001".U(4.W)
  final val Add  = "b0010".U(4.W)
  final val Sub  = "b0011".U(4.W)
  final val Div  = "b0100".U(4.W)
  final val Rem  = "b0101".U(4.W)
  final val Eq   = "b0110".U(4.W)
  final val Ne   = "b0111".U(4.W)
  final val Ge   = "b1000".U(4.W)
  final val Lt   = "b1001".U(4.W)
  final val Jump = "b1010".U(4.W)
  final val BNez = "b1011".U(4.W)
  final val Put  = "b1100".U(4.W)
  final val Lsl  = "b1101".U(4.W)
  final val And  = "b1110".U(4.W)
  final val Or   = "b1111".U(4.W)

  
  final val TSigned   = "b00".U(2.W)
  final val TUnsigned = "b01".U(2.W)
  final val TFloat    = "b10".U(2.W)
  final val TDouble   = "b11".U(2.W)
  
  final val W08 = "b00".U(2.W)
  final val W16 = "b01".U(2.W)
  final val W32 = "b10".U(2.W)
  final val W64 = "b11".U(2.W)
}
