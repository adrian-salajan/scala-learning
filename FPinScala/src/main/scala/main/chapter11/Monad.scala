package main.chapter11

import main.chapter12.ApplicativeR
import main.chapter6.State
import main.chapter7.Par
import main.chapter7.Par.Par

trait Monad[F[_]] extends ApplicativeR[F] {

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] // = join(map(ma)(f))
  def unit[A](a: => A): F[A]

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = x => flatMap(f(x))(b => g(b))
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  override def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.reverse.foldLeft(unit(List[A]())) {
      (z, fa) => map2(z, fa)( (li: List[A], a: A) => a :: li)
    }
  }


  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = unit(ms.filter(e => f(e) == unit(true)))

  def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t => flatMap(f(h))(b => if (b) map(filterM(t)(f))(h :: _) else filterM(t)(f))
  }

  def flatMapViaCompose[A,B](m: F[A])(f: A => F[B]): F[B] = compose[Unit, A, B](_ =>  m, f)()


  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def composeViaJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

}


object Monads {

  val forPar = new Monad[Par] {

    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

//  val forParser = new Monad[Parsers] {
//
//    override def unit[A](a: => A): Parsers[A] = Parsers.
//
//    override def flatMap[A, B](ma: Parsers[A])(f: (A) => Parsers[B]): Parsers[B] = ???
//  }

  val forOption = new Monad[Option] {

    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }

  val forStream = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val forList = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
  }

  type IntState[E] = State[Int, E]

  val forStateInt = new Monad[IntState] {

    override def unit[A](a: => A): IntState[A] = State.unit(a)

    override def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] = ma.flatMap(f)
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]) :Id[B] = f(value)
  }

  val forId = new Monad[Id] {

    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }

  //###########

  val F = stateMonad[Int]

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))



  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))(
      (acc,a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
      } yield (n, a) :: xs
    ).run(0)._1.reverse

  def zipWithIndex2[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))(
      (acc,a) =>
        acc.flatMap(xs => getState.flatMap(n => setState(n + 1).map(_ => (n, a) :: xs)))
    ).run(0)._1.reverse

  //############

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {

      def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader(r => {
        val a = st.run(r)
        f(a).run(r)
      })
    }
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
  type f[x] = Either[E, x]})#f] {

    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(r) => f(r)
      case Left(e) => Left(e)
    }

  }


}