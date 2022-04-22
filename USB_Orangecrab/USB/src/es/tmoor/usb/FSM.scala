package es.tmoor.usb

import chisel3._
import chisel3.util.Enum

class FSM(states: String*) {
  implicit def str2UInt(str: String): UInt = s(str)
  val eStates = Enum(states.size)
  val s = states.zip(eStates).toMap
  val state = Reg(UInt(eStates.last.getWidth.W))
}