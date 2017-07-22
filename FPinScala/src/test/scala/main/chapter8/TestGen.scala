package main.chapter8

import java.util.concurrent.{Executors, ExecutorService}

import main.Test
import main.chapter6.{SimpleRNG, RNG}
import main.chapter7.Par
import main.chapter8.Gen._
import main.chapter8.Prop._

class TestGen extends Test{

   "choose" should "work" in {
      val g = choose(1, 99).sample.run(SimpleRNG(999))._1

      assert(g >= 1 && g < 99)
   }

   "unit" should "work" in {
      val g = unit(7).sample.run(SimpleRNG(999))._1

      g shouldBe 7
   }

   "listOfN" should "work" in {
      val li = Gen.listOfN(10, choose(2, 5)).sample.run(SimpleRNG(97))._1

      all (li) should be < 5
      all (li) should be >=2
   }

   "listOfN viaFlatMap" should "work" in {
      val li = choose(2, 5).listOfN(choose(2, 4)).sample.run(SimpleRNG(97))._1

      all (li) should be < 5
      all (li) should be >=2

      assert(li.size >= 2 && li.size < 4)
   }

   "union" should "work" in {
      val u = union(unit(0), unit(1))

      val (first, rng) = u.sample.run(new DummyRng(List(4, 7, 4, 7))) // T F T F
      val (second, rng2) = u.sample.run(rng)
      val (third, _) = u.sample.run(rng2)

      first shouldBe 0
      second shouldBe 1
      third shouldBe 0

   }

   "prop &&" should "work" in {
      val b = Gen.boolean
      (forAll(b)(_ == true) && forAll(b)(_ == true)).run(1, 1, new DummyRng(List(2, 3, 4))) shouldBe Passed

      (forAll(b)(_ == true) && forAll(b)(_ == true)).run(1, 2, new DummyRng(List(2, 3, 4))) shouldBe Falsified("false", 1)

      (forAll(b)(_ == true).tag("L") && forAll(b)(_ == true).tag("R")).run(1, 3, new DummyRng(List(2, 4, 3, 3))) shouldBe Falsified("Lfalse", 2)

   }

   "prop ||" should "work" in {
      val b = Gen.boolean
      (forAll(b)(_ == false) || forAll(b)(_ == true)).run(1, 1, new DummyRng(List(2, 2, 2, 2))) shouldBe Passed

      (forAll(b)(_ == true) || forAll(b)(_ == false)).run(1, 2, new DummyRng(List(2, 2, 2, 2))) shouldBe Passed

      (forAll(b)(_ == false).tag("L") || forAll(b)(_ == false)).tag("R").run(1, 3, new DummyRng(List(3, 7, 2, 2))) shouldBe Falsified("Rtrue", 2)

   }

   "max on list" should "work" in {
      val smallInt = Gen.choose(-10,10)
      val maxProp = Prop.forAll(listOf1(smallInt)) { li =>
         val max = li.max
         !li.exists(_ > max)
      }

      Prop.run(maxProp) shouldBe Passed
   }

   "sorted list" should "work" in {
      val smallInt = Gen.choose(-10,10)
      val maxProp = Prop.forAll(listOf1(smallInt)) { li =>
         val sorted = li.sorted

         sorted.zip(sorted.tail).forall((t) => t._1 <= t._2)
      }

      Prop.run(maxProp) shouldBe Passed
   }

   "Par: law of mapping" should "work" in {
      val ES: ExecutorService = Executors.newCachedThreadPool

      val p1 = Prop.forAll(
         Gen.unit(Par.unit(1)))(i =>
            Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

      Prop.run(p1) shouldBe Passed
   }

   "Par: law of mapping2" should "work" in {
      val ES: ExecutorService = Executors.newCachedThreadPool

      val p1 = Prop.check {
         Par.map(Par.unit(1))(_ + 1)(ES) == Par.unit(2)(ES)
      }

      Prop.run(p1) shouldBe Proved
   }

   "Par: law of mapping3" should "work" in {
      val ES: ExecutorService = Executors.newCachedThreadPool

      val p3 = check {
         Par.equal(
            Par.map(Par.unit(1))(_ + 1),
            Par.unit(2)
         )(ES).get
      }

      Prop.run(p3) shouldBe Proved
   }

   "Par: law of mapping4" should "work" in {

      val p3 = checkPar {
         Par.equal(
            Par.map(Par.unit(1))(_ + 1),
            Par.unit(2)
         )
      }

      Prop.run(p3) shouldBe Passed
   }

   class DummyRng(r: List[Int]) extends RNG {
      override def nextInt: (Int, RNG) = (r.head, new DummyRng(r.tail))
   }
}

