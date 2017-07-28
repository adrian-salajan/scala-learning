/** ***********************************************************************
  * ULLINK CONFIDENTIAL INFORMATION
  * _______________________________
  *
  * All Rights Reserved.
  *
  * NOTICE: This file and its content are the property of Ullink. The
  * information included has been classified as Confidential and may
  * not be copied, modified, distributed, or otherwise disseminated, in
  * whole or part, without the express written permission of Ullink.
  * ***********************************************************************/
package main.chapter10

trait Foldable[F[_]] {

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concat[A](as: F[A])(m: Monoid[A]) = foldLeft(as)(m.zero)(m.op)

}


object Foldables {

  val forList = new Foldable[List] {

//    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
//      if (as.isEmpty) z
//      else foldLeft(as.tail)(f(z, as.head))(f)
//    }

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as)((a: A) => (b: B) => f(b,a))(Monoids.dual(Monoids.endoMonoid[B]))(z)

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(Monoids.endoMonoid[B])(z)


    override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B = ???
  }

  val forSeq = new Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = forList.foldLeft(as.toList)(z)(f)

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = forList.foldRight(as.toList)(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(m: Monoid[B]): B = forList.foldMap(as.toList)(f)(m)
  }
}