package module3.zio_homework
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]) =
    runApp.provideSomeLayer[Clock with Console with Random](RunningTimePrinter.live).exitCode
}
