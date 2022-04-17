import es.tmoor.dynarch.hardware._

object Main extends App {
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => new comb.ALU)
    )
  )
}