package es.tmoor.dynarch.hardware
import chisel3._
import Const._

class Typed extends Bundle {
  val d = UInt(64.W)
  val t = UInt(2.W)
  def isFloat    = t === TFloat
  def isDouble   = t === TDouble
  def isSigned   = t === TSigned
  def isUnsigned = t === TUnsigned
  def isInt = t(0)
}