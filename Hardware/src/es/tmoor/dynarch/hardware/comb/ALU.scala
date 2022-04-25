package es.tmoor.dynarch.hardware
package comb


import chisel3._
import chisel3.util.Mux1H

class ALU extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new Typed)
    val in2 = Input(new Typed)
    val out = Output(new Typed)
    val op = Input(UInt(4.W))
  })
  import io._, Const._
  
  def i2d(in: UInt): UInt = in
  def f2d(in: UInt): UInt = in
  
  def u2f(in: UInt): UInt = in
  def u2d(in: UInt): UInt = in

  def i2f(in: UInt): UInt = in
  def d2f(in: UInt): UInt = in
  
  def d2i(in: UInt): UInt = in
  def f2i(in: UInt): UInt = in

  val in1Upgrade = Wire(UInt(64.W))
  val in2Upgrade = Wire(UInt(64.W))
  val tUpgrade = Wire(UInt(2.W))

  def upgrade(t1: UInt, t2: UInt, tu: UInt, f1: UInt => UInt = identity, f2: UInt => UInt = identity) = (in: WhenContext) => {
    in.elsewhen (in1.t === t1 && in1.t === t2) {
      tUpgrade := tu
      in1Upgrade := f1(in1.d)
      in2Upgrade := f2(in2.d)
    }
  }
  Seq(
    upgrade(TDouble, TDouble, TDouble),
    upgrade(TDouble, TFloat, TDouble, f2=f2d),
    upgrade(TDouble, TSigned, TDouble, f2=i2d),
    upgrade(TDouble, TUnsigned, TDouble, f2=u2d),
    upgrade(TFloat, TDouble, TDouble, f1=f2d),
    upgrade(TFloat, TFloat, TFloat),
    upgrade(TFloat, TSigned, TFloat, f2=i2f),
    upgrade(TFloat, TUnsigned, TFloat, f2=u2f),
    upgrade(TSigned, TUnsigned, TUnsigned),
    upgrade(TUnsigned, TSigned,  TUnsigned),
  ).foldLeft(when(0.B) {
    tUpgrade := in1.t
    in1Upgrade := in1.d
    in2Upgrade := in2.d
  }) ((acc,v) => v(acc)).otherwise {
    tUpgrade := in1.t
    in1Upgrade := in1.d
    in2Upgrade := in2.d
  }

  val result = Reg(new Typed)
  out := result

  val unsignedALU = Module(new alu.Unsigned)
  unsignedALU.io.in1 := in1Upgrade
  unsignedALU.io.in2 := in2Upgrade
  unsignedALU.io.op := op
  
  val signedALU = Module(new alu.Signed)
  signedALU.io.in1 := in1Upgrade.asTypeOf(signedALU.io.in1)
  signedALU.io.in2 := in2Upgrade.asTypeOf(signedALU.io.in2)
  signedALU.io.op := op

  result := Mux1H(Seq(
    (tUpgrade === TUnsigned) -> unsignedALU.io.out,
    (tUpgrade === TSigned) -> signedALU.io.out
  ))
}