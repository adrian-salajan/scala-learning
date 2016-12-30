package main.chapter5

import java.util.Optional

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

   def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
   }

   def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
   }

   def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
   }

   def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])(
      (e, z) =>
         if (p(e)) Cons(() => e, () => z)
         else Empty
   )

   def headOptionFR: Option[A] = foldRight(None: Option[A])((e, z) => Option(e))

   def map[B](f: A => B) = foldRight(Empty: Stream[B])((e, z) => Cons(() => f(e), () => z))

   def filter(p: A => Boolean) = foldRight(Empty: Stream[A])((e, z) => if (p(e)) Cons(() => e, () => z) else z)

   def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((e, z) => Cons(() => e, () => z))

   def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])(
      (e, z) => f(e).append(z)
   )

   def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Cons(h, t) => Option((f(h()), t()))
      case _ => Option.empty
   }

   def takeUnfold(n: Int) = Stream.unfold((n, this)) {
      case (n, Cons(h, t)) if n > 0 => Option(h(), (n - 1, t()))
      case _ => None
   }

   def takeWhileUnfold(p: A => Boolean) = Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Option(h(), t())
      case _ => None
   }

   def zipWithUnfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Option(f(h(), hh()), (t(), tt()))
      case _ => None
   }

   def zipAllUnfold[B](s: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Option((Option(h()), Option(hh())), (t(), tt()))
      case (Empty, Cons(hh, tt)) => Option((None, Option(hh())), (Empty, tt()))
      case (Cons(h, t), Empty) => Option((Option(h()), None), (t(), Empty))
      case _ => None
   }


   def startsWith[A](seq: Stream[A]): Boolean = Stream.unfold((seq, this)) {
      case (Cons(h, t), Cons(hh, tt)) => if (h() == hh()) Option(true, (t(), tt())) else Option(false, (t(), tt()))
      case (Cons(h, t), Empty) => Option(false, (t(), Stream.empty))
      case _ => None
   }.forAll(r => r)


   def tails: Stream[Stream[A]] = Stream.unfold(this) {
      case c: Cons[A] => Option(c, c.drop(1))
      case _ => None
   }.append(Stream(Empty))

   def hasSubsequence[A](seq: Stream[A]): Boolean = {
      this.tails.foldRight(false)((s, r) => s.startsWith(seq) || r)
//    !this.tails.map(_.startsWith(seq)).forAll(r => !r)
   }

   def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      this.tails.map(_.foldRight(z)(f))

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

   def fromUnfold(n: Int) :Stream[Int] = unfold(n)(s => Option(s, s + 1))

   def constantUnfold[A](a :A) :Stream[A] = unfold(a)(a => Option(a, a))

   def ones:Stream[Int] = unfold(1)(_ => Option(1, 1))



}