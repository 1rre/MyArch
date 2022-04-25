package es.tmoor.dynarch.hardware
package comb.alu

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.MuxCase
import chisel3.util.Cat

class Unsigned extends Module {
  val io = IO(new Bundle {
    val in1 = Input (UInt(64.W))
    val in2 = Input (UInt(64.W))
    val op  = Input (UInt( 4.W))
    val out = Output(new Typed)
  })

  import io._, Const._
  
  out.t := TUnsigned

  def lslOut = {
    def shiftAm = in2 asTypeOf SInt(64.W) min 127.S max -128.S asTypeOf SInt(8.W)
    Mux(shiftAm >= 0.S, in1 << shiftAm.asUInt, in1 >> (-shiftAm).asUInt)
  }

  out.d := Mux1H(Seq(
    (op === Load) -> (in2),
    (op === Mul)  -> (in1  *  in2)(63, 0),
    (op === Add)  -> (in1  +  in2),
    (op === Sub)  -> (in1  -  in2),
    (op === Div)  -> (in1  /  in2),
    (op === Rem)  -> (in1  %  in2),
    (op === And)  -> (in1  &  in2),
    (op === Or)   -> (in1  |  in2),
    (op === Eq)   -> Cat(0.U(63.W), in1 === in2),
    (op === Ne)   -> Cat(0.U(63.W), in1 =/= in2),
    (op === Ge)   -> Cat(0.U(63.W), in1  >= in2),
    (op === Lt)   -> Cat(0.U(63.W), in1  <  in2),
    (op === Lsl)  -> (lslOut(63, 0)),
  ))

  
}