package main.chapter3

sealed trait MyList[+A]

case object Nili extends MyList[Nothing]



case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {
}
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

  def foldRight[A, B](l: MyList[A], z: B)(f: (A,B) => B): B = l match {
    case Nili => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def fill[A](n: Int, a: A): MyList[A] = {
    if (n <= 0) Nili
    else Cons(a, fill(n - 1, a))
  }

  def tail[A](l: MyList[A]) = l match {
    case Nili => throw new UnsupportedOperationException("Unsupported Nili.tail")
    case Cons(a, b) => b
  }

  def replaceHead[A](a: A, l: MyList[A]) = l match {
    case Nili => Cons(a, Nili)
    case Cons(_, y) => Cons(a, y)
  }

  def drop[A](n: Int, l: MyList[A]): MyList[A] = {
    if (n > 0) {
      l match {
        case Cons(h, t) => drop(n - 1, t)
        case Nili => throw new UnsupportedOperationException("Can't drop from Nili")
      }
    } else l
  }

  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
    case Nili => Nili
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
  }

  def init[A](l : MyList[A]): MyList[A] = l match {
    case Cons(h, Nili) => Nili
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: MyList[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
    case Nili => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

}
