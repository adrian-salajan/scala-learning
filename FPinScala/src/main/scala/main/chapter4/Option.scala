package main.chapter4

sealed trait Option[+A] {



   def map[B](f : A => B) :Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
   }

   def getOrElse[B >: A](value: => B) : B = this match {
      case Some(x) => x
      case None => value
   }

   def flatMap[B](f: A => Option[B]) :Option[B] = map(f).getOrElse(None)

   def orElse[B >: A](value: => Option[B]) :Option[B] =
      this match {
      case None => value
      case _ => this
   }

   def filter(p: A => Boolean): Option[A] = this match {
      case Some(x) => if (p(x)) this else None
      case _ => None
   }






}

case class Some[+A](some :A) extends Option[A]
case object None extends Option[Nothing]

object Option {
   def from[A](a : A): Option[A] = if (a == null) None else Some(a)
   def empty = None :Option[Nothing]

   def map2[A, B, C](a: Option[A], b: Option[B], f: (A, B) => C): Option[C] =
      a.flatMap( x => b.map(y => f(x, y)))

   //make the pattern more visible to spot
   def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C], f: (A, B, C) => D): Option[D] =
      a.flatMap( x => b.flatMap(y => c.map(z => f(x, y, z))))

   def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case head :: tail => head.flatMap(x => sequence(tail).map(y => x :: y))
      //case head :: tail => for { hh <- head; list <- sequence(tail)} yield  hh :: list
   }

   def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case head :: tail => f(head).flatMap(h => traverse(tail)(f).map(t => h :: t))
   }
   def sequenceT[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(e => e)




}

object Other {
   def mean(xs: Seq[Double]): Option[Double] = xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
   }
   def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean( xs.map(e => Math.pow(m - e, 2)) ))
   }
}

