import es.tmoor.usb._

object Main extends App {
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog") ++ args,
    Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => new TxPipeline)
    )
  )
}