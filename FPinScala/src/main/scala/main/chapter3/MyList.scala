package main.chapter3

sealed trait MyList[+A]

case object Nili extends MyList[Nothing]



case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]
// + variance notation makes: List[Dog] is considered a subtype of List[Animal]

object MyList {
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nili
    else Cons(as.head, apply(as.tail: _*))
  }


  def sum(ints: MyList[Int]): Int = ints match {
    case Nili => 0
    case Cons(a, as) => a + sum(as)
  }

  def fill[A](n: Int, a: A): MyList[A] = {
    if (n <= 0) Nili
    else Cons(a, fill(n - 1, a))
  }
}
