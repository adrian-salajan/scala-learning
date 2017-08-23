package main.chapter12

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait ApplicativeR[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    val x = 1
    apply(fa)(unit(f))
  }

  def apply[A,B](fa: F[A])(fab: F[A => B]): F[B] = {
    val x = 2
    map2(fa, fab)((a, ab) => ab(a))
  }

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val x = 3
    val fc = f.curried

    val partialF: F[B => C] = apply(fa)(unit(a => fc(a)))

    val r: F[C] = apply(fb)(partialF)
    r
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fcur = f.curried
    val fab = map2(fa, fb)((a, b) => fcur(a)(b))
    map2(fab, fc)((ab, c) => ab(c))
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc:F[C], fd: F[D])(f: (A, B, C, D) => E) = {
    val fcur = f.curried
    val fabc = map3(fa, fb, fc)((a,b, c) => fcur(a)(b)(c))
    map2(fabc, fd)((abc, d) => abc(d))
  }

  def product[G[_]](G: ApplicativeR[G]): ApplicativeR[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new ApplicativeR[({type f[x] = (F[x], G[x])})#f] {

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = (self.map(fa._1)(f), G.map(fa._2)(f))
    }
  }

  def compose[G[_]](G: ApplicativeR[G]):  ApplicativeR[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new ApplicativeR[({ type f[x] = F[G[x]]})#f] {

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = self.apply(fa)(self.unit(ga => G.map(ga)(f)))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldRight(unit(Map[K,V]()))((kv, z) => map2(kv._2, z)((v, z) => z.updated(kv._1, v)))

}

object Applicatives {

  def listApplic = new ApplicativeR[List] {

    override def unit[A](a: => A): List[A] = List(a)

    override def apply[A, B](fa: List[A])(fab: List[(A) => B]): List[B] = {
      val zipped: List[(A, (A) => B)] = fa.zip(fab)
       val r: List[B] = zipped.map(t => t._2(t._1))
      r
    }
  }

  def optionApplic = new ApplicativeR[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def apply[A, B](fa: Option[A])(fab: Option[(A) => B]): Option[B] = fa.flatMap(a => fab.map(f => f(a)))
  }


}