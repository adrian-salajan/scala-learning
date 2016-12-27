package main.chapter4

sealed trait Either[+E, +V] {

   def map[B](f: V => B): Either[E, B] = this match {
      case Right(v) => Right(f(v))
      case Left(e) => Left(e)
   }

   def orElse[EE >: E,B >: V](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => Right(v)
      case Left(e) => b
   }

   def flatMap[EE >: E, B](f: V => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
   }

   def map2[EE >: E, B, C](b: Either[EE, B])(f: (V, B) => C):  Either[EE, C] =
      flatMap(x => b.map(y => f(x, y)))




   def traverse[E, A, B](as: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] = ???
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+V](value: V) extends Either[Nothing, V]

object Either {
   def sequence[E, V](es: List[Either[E, V]]): Either[E, List[V]] = es match {
      case Nil => Right(Nil)
      case head :: tail => head match {
         case e: Left[E] => e
         case r: Right[V] => sequence(tail).map(li => r.value :: li)
      }
   }

   def traverse[E, V, B](as: List[V])(f: V => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case head :: tail => f(head).flatMap( v => traverse(tail)(f).map(tt => v :: tt))
   }

   def sequence2[E, V](es: List[Either[E, V]]): Either[E, List[V]] = traverse(es)(x => x)
}
