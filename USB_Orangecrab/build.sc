import mill._, scalalib._

object USB extends ScalaModule {
  def scalaVersion = "2.13.7" // Chisel isn't updated to 2.13.8 yet
    def scalacPluginIvyDeps = Agg (
        ivy"edu.berkeley.cs:::chisel3-plugin::3.5.0",
    )
    def ivyDeps = Agg (
        ivy"edu.berkeley.cs::chisel3::3.5.0",
    )

    object test extends Tests {
        def ivyDeps = Agg (
            ivy"edu.berkeley.cs::chisel3::3.5.0",
            ivy"edu.berkeley.cs::chiseltest::0.5.2",
        )

        def testFramework = "org.scalatest.tools.Framework"
    }
}
