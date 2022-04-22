package es.tmoor.usb

import chisel3._

class TxPipeline extends Module {
  val i = IO(new Bundle {
    val oe = Input(Bool())
    val iosg = Output(UInt(2.W))
    val iosp = Output(UInt(8.W))
  })
  val stateGrey = Reg(UInt(2.W))
  val syncPulse = Reg(UInt(8.W))
  i.iosp := syncPulse
  i.iosg := stateGrey
  val fsm = new FSM ("idle", "send sync", "send data", "stuff last bit") {
    when (state === "idle") {
      when (i.oe) {
        state := "send sync"
        syncPulse := (1 << 7).U
        stateGrey := 1.U
      } otherwise {
        stateGrey := 0.U
      }
    }

    when (state === "send sync") {
      syncPulse := syncPulse >> 1
      when (syncPulse(0)) {
        state := "send data"
        stateGrey := 3.U
      } otherwise {
        stateGrey := 1.U
      }
    }
  }
}