package es.tmoor.dynarch.hardware
import chisel3._

sealed abstract class Wishbone(dataWidth: Int) extends Bundle {
  val datI = Input(UInt(dataWidth.W))
  val datO = Output(UInt(dataWidth.W))
  val addr: UInt
  val we: Bool
  val sel: UInt
  val stb: Bool
  /** Waitrequest */
  val ack: Bool
  val cyc: Bool
}

class WishboneMaster(dataWidth: Int, addrWidth: Int, selWidth: Int = 1) extends Wishbone(dataWidth) {
  val addr = Output(UInt(addrWidth.W))
  val we = Output(Bool())
  val sel = Output(UInt(selWidth.W))
  val stb = Output(Bool())
  /** Waitrequest */
  val ack = Input(Bool())
  val cyc = Output(Bool())
}

class WishboneSlave(dataWidth: Int, addrWidth: Int, selWidth: Int = 1) extends Wishbone(dataWidth) {
  val addr = Input(UInt(addrWidth.W))
  val we = Input(Bool())
  val sel = Input(UInt(selWidth.W))
  val stb = Input(Bool())
  val ack = Output(Bool())
  val cyc = Input(Bool())
}
