package es.tmoor.dynarch.hardware
package comb

import chisel3._
import chisel3.util.Cat

class CPU(addrW: Int) extends Module {
  import Const._

  val io = IO(new Bundle {
    val bus = new WishboneMaster(16, addrW, 2)
    val memSize = Input(UInt(32.W))
  })
  val acc = Reg(new Typed)
  val reg = Module(new RegFile)
  val alu = Module(new comb.ALU)
  val pc = RegInit(ResetVector)
  val sp = RegInit(ResetVector + io.memSize)

  val mAddr = Reg(UInt(32.W))
  io.bus.addr := mAddr
  val byteEnable = Reg(UInt(2.W))
  io.bus.sel := byteEnable

  val dataR = RegInit(0.B)
  val dataW = RegInit(0.B)

  val dataRW = dataR | dataW
  io.bus.cyc := dataRW
  io.bus.stb := dataRW

  io.bus.we := dataW
  
  val fetch = RegInit(1.B)
  val decode = RegInit(0.B)
  val dataWait = RegInit(0.B)
  val exec = RegInit(0.B)
  val jump = RegInit(0.B)
  val writeback = RegInit(0.B)

  val opcode = io.bus.datI(15, 12)
  val typecode = io.bus.datI(9, 8)
  val isLiteral = io.bus.datI(11, 10) === "b00".U
  val isStack = io.bus.datI(11, 10) === "b01".U
  val isReg = io.bus.datI(11, 10) === "b10".U
  val isRef = io.bus.datI(11, 10) === "b11".U

  alu.io.in1 := acc
  alu.io.op := opcode

  val d2 = 0.U(64.W)
  val t2 = 0.U(3.W)

  alu.io.in2.d := d2
  alu.io.in2.t := t2  
  
  when (fetch) {
    dataR := 1.B
    dataW := 0.B
    mAddr := pc

    fetch := 0.B
    decode := 1.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

  when (decode) {
    fetch := 0.B
    decode := 0.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

  when (dataWait) {
    fetch := 0.B
    decode := 0.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

  when (exec) {
    acc := alu.io.out

    fetch := 1.B
    decode := 0.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

  when (jump) {
    pc := d2

    fetch := 1.B
    decode := 0.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

  when (writeback) {
    fetch := 1.B
    decode := 0.B
    dataWait := 0.B
    exec := 0.B
    jump := 0.B
    writeback := 0.B
  }

}