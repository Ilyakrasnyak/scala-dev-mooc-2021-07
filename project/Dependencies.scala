
import sbt.ModuleID
import sbt._

object Dependencies {

    lazy val ZioVersion = "1.0.4"
    lazy val PureconfigVersion = "0.12.3"

    lazy val zio: Seq[ModuleID] = Seq(
      "dev.zio" %% "zio" % ZioVersion,
      "dev.zio" %% "zio-test" % ZioVersion,
      "dev.zio" %% "zio-test-sbt" % ZioVersion,
      "dev.zio" %% "zio-macros" % ZioVersion
    )

    lazy val pureconfig: Seq[ModuleID] = Seq(
      "com.github.pureconfig" %% "pureconfig" % PureconfigVersion,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureconfigVersion
    )
}
