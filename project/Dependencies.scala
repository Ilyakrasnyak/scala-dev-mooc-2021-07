
import sbt.ModuleID
import sbt._

object Dependencies {

    lazy val ZioVersion = "1.0.4"

    lazy val zio: Seq[ModuleID] = Seq(
      "dev.zio" %% "zio" % ZioVersion,
      "dev.zio" %% "zio-test" % ZioVersion,
      "dev.zio" %% "zio-test-sbt" % ZioVersion,
      "dev.zio" %% "zio-macros" % ZioVersion,
      "dev.zio" %% "zio-config" % ZioVersion,
      "dev.zio" %% "zio-config-magnolia" % ZioVersion
    )
}
