package module3

import zio._
import zio.clock._
import zio.console._
import zio.macros.accessible
import zio.random._
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.duration._

import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {

  val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)

  def printEffectRunningTime[R, E, A](
      zio: ZIO[R, E, A]
  ): ZIO[Clock with Console with R, E, A] = for {
    start <- currentTime
    z <- zio
    end <- currentTime
    _ <- putStrLn(s"Running time: ${end - start}")
  } yield z

  /** 1.
    * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
    * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
    */

  lazy val guessProgram: ZIO[Console with Random, Throwable, Unit] = for {
    _ <- putStrLn("Try to guess a number from 1 to 3")
    userInput <- getStrLn
    randInt <- nextIntBetween(1, 3)
    answer <- createAnswer(userInput, randInt)
    _ <- putStrLn(answer)
    _ <- guessProgram
  } yield ()

  def createAnswer(input: String, randInt: Int): ZIO[Any, Throwable, String] =
    for {
      int <- ZIO(input.toIntOption)
    } yield int match {
      case Some(value) if 1 <= value && value <= 3 =>
        value match {
          case value if value == randInt => "You won!"
          case _                         => "You lose!"
        }
      case Some(_) => "Enter a number from one to three"
      case None    => "Can't find a number in a row"
    }

  /** 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
    */

  def doWhile[R, E, A](
      cond: A => Boolean
  )(effect: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    effect.flatMap(res => doWhile(cond)(effect).unless(cond(res)))
  }

  /** 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
    * и выведет его в консоль
    * Используйте эффект "load" из пакета config
    */

  case class MyConfig(port: Int, dburl: String)
  val defaultConfig: MyConfig = MyConfig(8080, "db@url:12345")
  val myConfigDescriptor: ConfigDescriptor[MyConfig] = descriptor[MyConfig]

  def loadConfigOrDefault: ZLayer[Any, Nothing, Has[MyConfig]] =
    ZConfig
      .fromPropertiesFile("filepath", myConfigDescriptor)
      .catchAll(ZLayer.succeed(defaultConfig))

  /** 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
    * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
    * на изменение этих сигнатур
    */

  /** 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
    * Используйте сервис zio Random
    */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = for {
    _ <- ZIO.sleep(1 second)
    randInt <- nextIntBetween(1, 10)
  } yield randInt

  /** 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
    */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] =
    List.fill(10)(eff)

  /** 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
    * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
    * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
    */

  lazy val app: ZIO[
    Random with Clock with Console,
    Nothing,
    Int
  ] =
    printEffectRunningTime(
      for {
        res <- ZIO.collectAll(effects)
        s = res.sum
        _ <- putStrLn(s.toString)
      } yield s
    )

  /** 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
    */

  lazy val appSpeedUp: ZIO[Clock with Console with Random, Nothing, Int] =
    printEffectRunningTime(
      for {
        res <- ZIO.collectAllPar(effects)
        s = res.sum
        _ <- putStrLn(s.toString)
      } yield s
    )

  /** 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
    * молжно было использовать аналогично zio.console.putStrLn например
    */

  type RunningTimePrinter = Has[RunningTimePrinter.Service]

  @accessible
  object RunningTimePrinter {

    trait Service {

      val currentTime: URIO[Clock, Long]

      def printEffectRunningTime[R, E, A](
          zio: ZIO[R, E, A]
      ): ZIO[Clock with Console with R, E, A]
    }

    val live: ULayer[Has[Service]] = ZLayer.succeed(
      new Service {

        val currentTime: URIO[Clock, Long] =
          clock.currentTime(TimeUnit.SECONDS)

        def printEffectRunningTime[R, E, A](
            zio: ZIO[R, E, A]
        ): ZIO[Clock with Console with R, E, A] =
          for {
            start <- currentTime
            z <- zio
            end <- currentTime
            _ <- putStrLn(s"Running time: ${end - start}")
          } yield z
      }
    )
  }

  /** 6.
    * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
    */

  lazy val appWithTimeLogg: ZIO[
    Clock with Console with Random with RunningTimePrinter,
    Nothing,
    Int
  ] =
    RunningTimePrinter.printEffectRunningTime(
      for {
        res <- ZIO.collectAllPar(effects)
        s = res.sum
        _ <- putStrLn(s.toString)
      } yield s
    )

  /** Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[
    Clock with Console with Random with RunningTimePrinter,
    Nothing,
    Int
  ] = appWithTimeLogg

}
