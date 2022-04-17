package es.tmoor.dynarch.hardware
import chisel3._

class RegFile extends Module {
  val io = IO(new Bundle {
    val wrEn = Input(Bool())
    val addr = Input(UInt(8.W))
    val dataIn = Input(UInt(64.W))
    val dataOut = Output(UInt(64.W))
  })
  
  val reg = Reg(Vec(128, UInt(64.W)))
}