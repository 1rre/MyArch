package es.tmoor.dynarch.hardware

import chisel3._

class DynTop(addrW: Int) extends Module {
  val cpu = Module(new comb.CPU(addrW))
}