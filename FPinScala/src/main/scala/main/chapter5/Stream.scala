package main.chapter5

sealed trait Stream[+A] {

   def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
   }

   def take(n: Int): Stream[A] = this match {
      case Empty => Stream.empty
      case Cons(h, t) if n == 0 => Stream.empty
      case Cons(h, t) => Cons(() => h(), () => t().take(n - 1))
   }

   def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
   }

   def takeWhile(p: A => Boolean) :Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
   }

   def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
   }

   def foldRight[B](z: B)(f: (A, => B) => B ) : B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
   }

   def takeWhileFR(p: A => Boolean) :Stream[A] = foldRight(Empty: Stream[A])(
      (e, z) =>
         if (p(e)) Cons(() => e, () => z)
         else Empty
   )

   def headOptionFR: Option[A] = foldRight(None :Option[A])((e, z) => Option(e))

   def map[B](f: A => B) = foldRight(Empty :Stream[B])((e, z) => Cons(() => f(e), () => z))

   def filter(p: A => Boolean) = foldRight(Empty: Stream[A])((e, z) => if (p(e)) Cons(() => e, () => z) else z)

   def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((e, z) => Cons(() => e, () => z))

   def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty :Stream[B])(
      (e, z) => f(e).append(z)
   )

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
   def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
   }

   def empty[A]: Stream[A] = Empty

   def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

   def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))

   def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

   def fibs: Stream[Int] = {
      def fib(a: Int, b:Int) :Stream[Int] = Cons(() => a , () => fib(b, a + b))
      fib(0, 1)
   }

   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
         case None => Stream.empty
         case Some((next, state)) => cons(next, unfold(state)(f))
      }
   }

   def fibsUnfold: Stream[Int] = unfold((0, 1))(s => Option(s._1, (s._2, s._1 + s._2)))


}