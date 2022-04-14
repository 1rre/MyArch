import es.tmoor.dynarch.hardware.CPU

object Main extends App {
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => CPU)
    )
  )
}