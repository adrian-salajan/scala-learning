package main.chapter10

import main.chapter7.Par
import main.chapter7.Par.Par
import main.chapter8.{Gen, Prop}

trait Monoid[A] {
  def zero: A
  def op(a: A, b: A): A
}

object Monoids {

  val intAddition = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a: Int, b: Int): Int = a + b
  }

  val intMultiplication = new Monoid[Int] {
    override def zero: Int = 1
    override def op(a: Int, b: Int): Int = a * b
  }

  val booleanOr = new Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a: Boolean, b: Boolean): Boolean = a || b
  }

  val booleanAnd = new Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a: Boolean, b: Boolean): Boolean = a && b
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def zero: Option[A] = None
    override def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)
  }

  def endoMonoid[A] = new Monoid[A => A] {

    override def zero: (A) => A = (x:A) => x

    override def op(a: (A) => A, b: (A) => A): (A) => A = a compose b
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {

    override def op(a: A, b: A): A = m.op(b, a)

    override def zero: A = m.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val identity1 = Prop.forAll(gen)(a => m.op(m.zero, a) == a).tag("id1")
    val identity2 = Prop.forAll(gen)(a => m.op(a, m.zero) == a).tag("id2")
    val comm = Prop.forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a)).tag("comm")
    val assoc = Prop.forAll(gen)(a => m.op(m.op(a, m.zero), a) == m.op(a, m.op(m.zero, a)))
    identity1 && identity2 && comm && assoc
  }

  def monoidLaws2[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val identity1 = Prop.forAll(gen)(a => m.op(m.zero, a) == a).tag(" id1 ")
    val identity2 = Prop.forAll(gen)(a => m.op(a, m.zero) == a).tag(" id2 ")

    val assoc = Prop.forAll(gen.flatMap(a => gen.flatMap(b => gen.map(c => (a, b, c)))))((p: ((A, A, A))) =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)).tag(" assoc ")

    identity1 && identity2 && assoc
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldLeft[A](as: List[A])(z: A)(f: (A, A) => A): A = {
    val mon = new Monoid[A] {
      override def zero: A = z
      override def op(a: A, b: A): A = f(a, b)
    }
    foldMap(as, mon)(identity)
  }


  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v(0))
    else {
      val (a, b) = v.splitAt(v.size / 2)
      m.op(
        foldMapV(a, m)(f),
        foldMapV(b, m)(f)
      )
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {

    override def op(a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    if (v.size == 1) Par.unit(f(v(0)))
    else {
      val (a, b) = v.splitAt(v.size / 2)
      par(m).op(
        parFoldMap(a, m)(f),
        parFoldMap(b, m)(f)
      )
    }
  }

  //does not respect monoid laws
  def isOrderedInt = new Monoid[(Int, Boolean)] {

    override def zero: (Int, Boolean) = (Integer.MIN_VALUE, true)

    override def op(a: (Int, Boolean), b: (Int, Boolean)): (Int, Boolean) = {
      val (aa, za) = a
      val (bb, zb) = b

      if (aa < bb)      (bb, za && zb)
      else if (aa > bb) (aa, false)
      else              (aa, za && zb)
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def zero: WC = Stub("")

    override def op(a: WC, b: WC): WC = {
      (a, b) match {
        case (Stub(x), Stub(y)) => Stub(x + y)
        case (Part(q, w, e), Stub(c)) => Part(q, w, e + c)
        case (Stub(c), Part(q, w, e)) => Part(c + q, w, e)
        case (Part(q, w, e), Part(z, x, c)) => Part(q, w + x + (if ((e + z).isEmpty) 0 else 1), c)
      }
    }
  }

  def countWords(s: String): Int = {
      def wc(c: Char): WC = {
        if (c.isWhitespace) Part("", 0, "")
        else Stub(c.toString)
      }

    foldMapV(s, wcMonoid)(wc)

    -1
  }

  def listConcatMonoid[A] = new Monoid[List[A]] {
    override def zero: List[A] = List()

    override def op(a: List[A], b: List[A]): List[A] = a ::: b
  }

  def mapViaFoldMap[A, B](as: List[A])(f: A => B) : List[B] = foldMap(as, listConcatMonoid[B])(a => List(f(a)))

  def productMonoid[A,B](m: Monoid[A], n: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {

    override def zero: (A, B) = (m.zero, n.zero)

    override def op(a: (A, B), b: (A, B)): (A, B) = (m.op(a._1, b._1), n.op(a._2, b._2))

  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {

      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) = {
        val ab: Set[K] = (a.keySet ++ b.keySet)
        ab.foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
      }
    }

  def functionMonoid[A,B](bMon: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {

    override def zero: A => B = a => bMon.zero

    override def op(af: A => B, bf: A => B): A => B = (aa:A) => bMon.op(af(aa), bf(aa))

  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    Foldables.forSeq.foldMap(as)(e => Map(e -> 1))(mapMergeMonoid(Monoids.intAddition))
  }


} //obj monoids
