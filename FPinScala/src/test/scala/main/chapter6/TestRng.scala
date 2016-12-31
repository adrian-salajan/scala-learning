package main.chapter6

import main.Test
import main.chapter6.RNG.Rand

class TestRng extends Test{

   "nonNegativeInt" should "work" in {
      val rng = SimpleRNG(1)
      RNG.nonNegativeInt(rng)._1 shouldBe 384748
      RNG.nonNegativeInt(rng)._1 shouldBe 384748
      RNG.nonNegativeInt(rng)._1 shouldBe 384748
      RNG.nonNegativeInt(RNG.nonNegativeInt(rng)_2)._1 should be >= 0
   }

   "double random" should "work" in {
      val rng = SimpleRNG(2)

      val (n ,r) = RNG.double(rng)
      assert(n >= 0 && n < 1)

      val (m ,rr) = RNG.double(r)
      assert(m >= 0 && m < 1)
   }

   "ints" should "generate list of random ints" in {
      val (l, r) = RNG.ints(3)(SimpleRNG(2))
      l.size shouldBe 3
   }

   "doubleMap" should "work" in {
      val d = RNG.doubleMap(SimpleRNG(2))._1
      assert(d >= 0 && d < 1)
   }

   "maps" should "work" in {
      val r1:Rand[Int] = RNG.map(RNG.nonNegativeInt)(i => i % 3)
      val r2:Rand[Int] = RNG.map(RNG.nonNegativeInt)(i => i % 4)

      val r3 = RNG.map2(r1, r2)(_ + _)

      val a = r3(SimpleRNG(2))._1
      assert(a >= 0 && a < 7)

      val r4 = RNG.map2(r1, r2)(_ * _)
      val b = r4(SimpleRNG(2))._1

      assert(a >= 0 && a < 12)
   }

   "sequence" should "work" in {
      val r = SimpleRNG(3)
      RNG.sequence(List(RNG.int, RNG.doubleMap))(r)._1 shouldBe List(RNG.int(r)_1, RNG.doubleMap(RNG.int(r)_2)_1)

      RNG.sequenceFold(List(RNG.int, RNG.doubleMap))(r)._1 shouldBe List(RNG.int(r)_1, RNG.doubleMap(RNG.int(r)_2)_1)
   }

   "intsSequence" should "generate list of random ints" in {
      val (l,r) = RNG.intsSequence(3)(SimpleRNG(2))
      l.size shouldBe 3
   }

   "flatMap" should "work" in {
      val rand = SimpleRNG(3)
      val intUnit :Rand[Int] = RNG.unit[Int](2)
      val rd = RNG.flatMap(intUnit)(i => RNG.double)
      val d = rd(rand)._1
      assert(d >= 0 && d < 1)
      assert(rd(rand)._1 == rd(rand)._1)
   }

   "nonNegativeLessThan" should "work" in {
      val rand = SimpleRNG(3)
      val r1 = RNG.nonNegativeLessThan(10)(rand)._1
      assert(r1 >= 0 && r1 < 10)
      val r2 = RNG.nonNegativeLessThan(5)(rand)._1
      assert(r2 >= 0 && r2 < 5)
   }

   "mapsF" should "work" in {
      val r1:Rand[Int] = RNG.mapF(RNG.nonNegativeInt)(i => i % 3)
      val r2:Rand[Int] = RNG.mapF(RNG.nonNegativeInt)(i => i % 4)

      val r3 = RNG.map2F(r1, r2)(_ + _)

      val a = r3(SimpleRNG(2))._1
      assert(a >= 0 && a < 7)

      val r4 = RNG.map2F(r1, r2)(_ * _)
      val b = r4(SimpleRNG(2))._1

      assert(a >= 0 && a < 12)
   }
}
