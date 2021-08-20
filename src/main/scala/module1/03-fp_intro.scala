package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant


/**
 * referential transparency
 */
object referential_transparency {


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification {
    case class Email(email: String, text: Html) extends Notification

    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService {
    def sendNotification(notification: Notification): Unit

    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService {

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService {

    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(UUID.randomUUID().toString(), abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

    def registerAbiturient2(uuid: UUID, abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(uuid.toString(), abiturientDTO.email, abiturientDTO.fio)
      abiturient
    }

  }
}


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if (n == 1) 1
    else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if (n == 1) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

  def fib(n: Int): Int = ???

}

object hof {

  def printFactorialResult(r: Int) = println(s"Factorial result is ${r}")

  def printFibonacciResult(r: Int) = println(s"Fibonacci result is ${r}")

  def printResult[T](r: T, funcName: String) = println(s"$funcName result is ${r}")

  def printRunningTimeFunc1[A, B](a: A)(f: A => B): Unit = {
    val current = Instant.now().toEpochMilli()
    f(a)
    val current2 = Instant.now().toEpochMilli()
    println(current2 - current)
  }


  // Follow type implementation
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val r: Int => Int = partial(1, sum)

}


/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  case class Some[+A](v: A) extends Option[A]

  case object None extends Option[Nothing]

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("Get on empty Option")
    }

    def getOrElse[TT >: T](b: TT): TT = this match {
      case Some(v) => v
      case None => b
    }

    def map[B](f: T => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def printIfAny(): Unit = this match {
      case Some(v) => println(v)
    }

    def zip[A >: T, B](other: Option[B]): Option[(T, B)] =
      if (this.isEmpty || other.isEmpty) None
      else Some(this.get, other.get)

    def filter(cond: T => Boolean): Option[T] = this match {
      case Some(v) if cond(v) => this
      case _ => None
    }
  }
}

object list {

  trait MyList[+T] {

    def empty[A >: T]: MyList[A] = MyNil

    def ::[A >: T](elem: A): Cons[A] = Cons(elem, this)

    def mkString(sep: String): String = {
      @tailrec
      def loop(MyList: MyList[T], result: String): String = MyList match {
        case Cons(head, tail: Cons[T]) => loop(tail, result + head.toString + sep)
        case Cons(head, tail: MyNil.type) => loop(tail, result + head.toString)
        case MyNil => result
      }

      loop(this, "")
    }

    def make[A >: T](args: A*): MyList[A] = args.foldRight(this.empty[A])((a: A, b: MyList[A]) => Cons(a, b))

    def reverse(): MyList[T] = {
      @tailrec
      def loop(l: MyList[T], r: MyList[T]): MyList[T] = l match {
        case Cons(head, tail) => loop(tail, Cons(head, r))
        case MyNil => r
      }

      loop(this, MyNil)
    }

    def map[A](f: T => A): MyList[A] = {
      def loop(l: MyList[T]): MyList[A] = l match {
        case Cons(head, tail) => Cons(f(head), loop(tail))
        case MyNil => MyNil
      }

      loop(this)
    }

    def filter(f: T => Boolean): MyList[T] = {
      def loop(l: MyList[T]): MyList[T] = l match {
        case Cons(head, tail) if f(head) => Cons(head, loop(tail))
        case Cons(head, tail) if !f(head) => loop(tail)
        case MyNil => MyNil
      }

      loop(this)
    }

    def incList(list: MyList[Int]): MyList[Int] = list.map(_ + 1)

    def shoutString(list: MyList[String]): MyList[String] = list.map("!" + _)

  }

  case class Cons[+T](head: T, tail: MyList[T]) extends MyList[T]

  case object MyNil extends MyList[Nothing]

}
