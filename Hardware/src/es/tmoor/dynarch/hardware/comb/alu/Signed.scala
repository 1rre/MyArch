package es.tmoor.dynarch.hardware
package comb.alu

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.MuxCase
import chisel3.util.Cat

class Signed extends Module {
  val io = IO(new Bundle {
    val in1 = Input (SInt(64.W))
    val in2 = Input (SInt(64.W))
    val op  = Input (UInt( 4.W))
    val out = Output(new Typed)
  })

  import io._, Const._

  val outS = Wire(SInt(64.W))
  out.d := outS.asTypeOf(UInt(64.W))

  def lslOut = {
    def shiftAm = in2 asTypeOf SInt(64.W) min 127.S max -128.S asTypeOf SInt(8.W)
    Mux(shiftAm >= 0.S, in1 << shiftAm.asUInt, in1 >> (-shiftAm).asUInt) 
  }

  outS := Mux1H(Seq(
    (op === Load) -> (in2),
    (op === Mul)  -> (in1  *  in2)(63, 0).asTypeOf(outS),
    (op === Add)  -> (in1  +  in2),
    (op === Sub)  -> (in1  -  in2),
    (op === Div)  -> (in1  /  in2),
    (op === Rem)  -> (in1  %  in2),
    (op === And)  -> (in1  &  in2),
    (op === Or)   -> (in1  |  in2),
    (op === Eq)   -> Cat(0.U(63.W), in1 === in2).asTypeOf(outS),
    (op === Ne)   -> Cat(0.U(63.W), in1 =/= in2).asTypeOf(outS),
    (op === Ge)   -> Cat(0.U(63.W), in1  >= in2).asTypeOf(outS),
    (op === Lt)   -> Cat(0.U(63.W), in1  <  in2).asTypeOf(outS),
    (op === Lsl)  -> (lslOut(63, 0).asTypeOf(outS)),
  ))

  out.t := MuxCase(TSigned, Seq(
    (op === Eq) -> TUnsigned,
    (op === Ne) -> TUnsigned,
    (op === Ge) -> TUnsigned,
    (op === Lt) -> TUnsigned
  ))
  
}