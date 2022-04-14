package es.tmoor.dynarch.hardware

import chisel3._

object CPU extends chisel3.Module {
  val ddram = IO(new Bundle {
    val a = Output(UInt(16.W))
    val ba = Output(UInt(3.W))
    val ras_n = Output(Bool())
    val cas_n = Output(Bool())
    val we_n = Output(Bool())
    val cs_n = Output(Bool())
    val dm = Output(UInt(2.W))
    val dq_in = Input(UInt(16.W)) 
    val dq_out = Output(UInt(16.W)) 
    val dqs_p_in = Input(UInt(2.W))
    val dqs_p_out = Output(UInt(2.W))
    val clk_p = Output(Clock())
    val cke = Output(Bool())
    //val odt = Output(Bool())
    val reset_n = Output(Reset())
    //val vccio = Output(UInt(5.W))
    //val gnd = Output(UInt(2.W))
  })
  val rgb_led0 = IO(new Bundle {
    val r = Output(Bool())
    val g = Output(Bool())
    val b = Output(Bool())
  })
  val usr = IO(new Bundle {
    val btn = Input(Bool())
  })
  val usb = IO(new Bundle {
    val d_p_in = Input(Bool())
    val d_p_out = Output(Bool())
    val d_n_in = Input(Bool())
    val d_n_out = Output(Bool())
    val pullup_in = Input(Bool())
    val pullup_out = Output(Bool())
  })
  val rst = IO(new Bundle {
    val n = Output(Bool())
  })
  val spiflash4x = IO(new Bundle {
    val cs_n = Output(Bool())
    val dq_in = Input(UInt(4.W))
    val dq_out = Output(UInt(4.W))
  })

  ddram.a := DontCare
  ddram.ba := DontCare
  ddram.ras_n := DontCare
  ddram.cas_n := DontCare
  ddram.we_n := DontCare
  ddram.cs_n := DontCare
  ddram.dm := DontCare
  ddram.dq_out := DontCare
  ddram.dqs_p_out := DontCare
  ddram.clk_p := clock
  ddram.cke := false.B
  ddram.reset_n := true.B

  usb.d_n_out := DontCare
  usb.d_p_out := DontCare
  usb.pullup_out := DontCare

  val uR = RegInit(0.B)
  val uG = RegInit(0.B)
  val uB = RegInit(1.B)

  when (usr.btn) {
    uR := uB
    uG := uR
    uB := uG
  }

  rgb_led0.r := uR
  rgb_led0.g := uG
  rgb_led0.b := uB

  rst.n := 0.B

  spiflash4x.cs_n := DontCare
  spiflash4x.dq_out := DontCare
}