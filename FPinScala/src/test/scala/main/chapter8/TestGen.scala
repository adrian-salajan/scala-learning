package main.chapter8

import main.Test
import main.chapter6.{SimpleRNG, RNG}
import main.chapter8.Gen.{unit, union, choose}
import main.chapter8.Prop.{Falsified, forAll, Passed}

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
      (forAll(b)(_ == true) && forAll(b)(_ == true)).run(1, new DummyRng(List(2, 3, 4))) shouldBe Passed

      (forAll(b)(_ == true) && forAll(b)(_ == true)).run(2, new DummyRng(List(2, 3, 4))) shouldBe Falsified("false", 1)

      (forAll(b)(_ == true).tag("L") && forAll(b)(_ == true).tag("R")).run(3, new DummyRng(List(2, 4, 3, 3))) shouldBe Falsified("Lfalse", 2)

   }

   "prop ||" should "work" in {
      val b = Gen.boolean
      (forAll(b)(_ == false) || forAll(b)(_ == true)).run(1, new DummyRng(List(2, 2, 2, 2))) shouldBe Passed

      (forAll(b)(_ == true) || forAll(b)(_ == false)).run(2, new DummyRng(List(2, 2, 2, 2))) shouldBe Passed

      (forAll(b)(_ == false).tag("L") || forAll(b)(_ == false)).tag("R").run(3, new DummyRng(List(3, 7, 2, 2))) shouldBe Falsified("Rtrue", 2)

   }

   class DummyRng(r: List[Int]) extends RNG {
      override def nextInt: (Int, RNG) = (r.head, new DummyRng(r.tail))
   }
}

