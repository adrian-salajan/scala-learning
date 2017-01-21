package main.chapter8

import main.chapter6.{State, RNG}
import main.chapter8.Gen.unit

case class Gen[A](sample: State[RNG, A]) {

   def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

   def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))

   def unsized: SGen[A] = SGen(_ => this)

}
case class SGen[A](forSize: Int => Gen[A]) {

   def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).unsized.flatMap(f).forSize(n))

   def listOf(g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(unit(n)))
}


object Gen {

   def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      val s: State[RNG, Int] = State(RNG.nonNegativeInt)
      Gen[Int](s.map(_ % (stopExclusive-start) + start))
   }

   def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

   def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

   def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

   def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) g1 else g2)

   def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val dg = Gen(State(RNG.double))
      val w1 = g1._2.abs
      val w2 = g2._2.abs
      val weight = w1 / (w1 + w2)
      dg.flatMap(d => if (d < weight) g1._1 else g2._1)
   }





}
