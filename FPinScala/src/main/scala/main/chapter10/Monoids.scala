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

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val identity1 = Prop.forAll(gen)(a => m.op(m.zero, a) == a).tag("id1")
    val identity2 = Prop.forAll(gen)(a => m.op(a, m.zero) == a).tag("id2")
    val comm = Prop.forAll(gen)(a => m.op(a, m.zero) == m.op(m.zero, a)).tag("comm")
    val assoc = Prop.forAll(gen)(a => m.op(m.op(a, m.zero), a) == m.op(a, m.op(m.zero, a)))
    identity1 && identity2 && comm && assoc
  }

  def monoidLaws2[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val identity1 = Prop.forAll(gen)(a => m.op(m.zero, a) == a).tag("id1")
    val identity2 = Prop.forAll(gen)(a => m.op(a, m.zero) == a).tag("id2")

    val assoc = Prop.forAll(gen.flatMap(a => gen.flatMap(b => gen.map(c => (a, b, c)))))((p: ((A, A, A))) =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)).tag("assoc")

    identity1 && identity2 && assoc
  }
}
