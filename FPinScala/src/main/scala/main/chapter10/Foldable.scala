package main.chapter10

import main.chapter3.Tree

import scala.annotation.tailrec
import scala.collection.immutable.Stream.{Empty, cons}

trait Foldable[F[_]] {

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concat[A](as: F[A])(m: Monoid[A]) = foldLeft(as)(m.zero)(m.op)

}


object Foldables {

  val forList = new Foldable[List] {

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a: A) => (b: B) => f(b,a))(Monoids.dual(Monoids.endoMonoid[B]))(z)

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoids.endoMonoid[B])(z)


    override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B = {
      @tailrec
      def foldMap[X, Y](b: Y, as: List[X])(f: (X) => Y)(m: Monoid[Y]): Y = {
        as match {
          case Nil => b
          case h :: tail => foldMap(m.op(b, f(h)), tail)(f)(m)
        }
      }
      foldMap(m.zero, as)(f)(m)
    }

  }
  val forSeq = new Foldable[IndexedSeq] {

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a: A) => (b: B) => f(b,a))(Monoids.dual(Monoids.endoMonoid[B]))(z)

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoids.endoMonoid[B])(z)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(m: Monoid[B]): B = {
      Monoids.foldMapV(as, m)(f)
    }
  }

  val forStream = new Foldable[Stream] {

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a: A) => (b: B) => f(b,a))(Monoids.dual(Monoids.endoMonoid[B]))(z)

    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoids.endoMonoid[B])(z)

    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(m: Monoid[B]): B = {
      @tailrec
      def foldMap[A, B](b: B, as: Stream[A])(f: (A) => B)(m: Monoid[B]): B = {
        as match {
          case Empty => b
          case cons(h, tail) => foldMap(m.op(b, f(h)), tail)(f)(m)
        }
      }
      foldMap(m.zero, as)(f)(m)
    }
  }

  val forTree = new Foldable[Tree] {
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a: A) => (b: B) => f(b,a))(Monoids.dual(Monoids.endoMonoid[B]))(z)

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoids.endoMonoid[B])(z)

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B = {
      Tree.fold(as)(f)(m.op)
    }
  }
}