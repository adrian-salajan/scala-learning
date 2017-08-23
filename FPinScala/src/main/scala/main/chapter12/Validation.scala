package main.chapter12

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {

  def applic[E] = new ApplicativeR[({type f[x] = Validation[E, x]})#f] {

    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = fa match {
      case Success(s1) => fb match {
        case Success(s2) => Success(f(s1, s2))
        case Failure(e, v) => Failure(e, v)
      }
      case Failure(se, sev) => fb match {
        case Success(s2) => Failure(se, sev)
        case Failure(e, v) => Failure(se, e +: (sev ++ v))
      }
    }
  }
}
