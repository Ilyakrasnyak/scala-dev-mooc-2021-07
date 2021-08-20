
import s.{Cons, MyNil}

import scala.annotation.tailrec
object s {
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
  }

  case class Cons[+T](head: T, tail: MyList[T]) extends MyList[T]

  case object MyNil extends MyList[Nothing]

}
println(Cons(1, Cons(2, MyNil)).reverse())
