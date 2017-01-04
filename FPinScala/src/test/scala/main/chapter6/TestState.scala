package main.chapter6

import main.Test

class TestState extends Test {

   val rng: RNG = SimpleRNG(2)
   val random1: Int = rng.nextInt._1
   val random2: Int = rng.nextInt._2.nextInt._1

   "unit" should "work" in {
      State.unit(3).run(SimpleRNG(2)) shouldBe (3, SimpleRNG(2))


   }

   "map" should "work" in {
      State.unit(3).map(_ * 2).run(SimpleRNG(2)) shouldBe (6, SimpleRNG(2))

      State[RNG, Int](s => s.nextInt).map(_ * 2).run(rng)._1 shouldBe random1 * 2
   }

   "map2" should "work" in {
      State.unit[String, Int](3).map2(State.unit[String, Int](4))(_ + _).run("x") shouldBe State.unit(7).run("x")

      val map2 = State[RNG, Int](s => s.nextInt).map2(State[RNG, Int](s => s.nextInt))((a, b) => a + b + 1)
      map2.run(rng)._1 shouldBe random1 + random2 + 1
   }

   "flatMap" should "work" in {
      State.unit[String, Int](3).flatMap(x => State.unit[String, Int](x * 2)).run("x") shouldBe State.unit(6).run("x")

      val fp = State[RNG, Int](s => s.nextInt)
                  .flatMap(x => State[RNG, String](rng => (x.toString, SimpleRNG(x))))

      fp.run(rng)._1 shouldBe random1.toString()
   }

   "sequence" should "work" in {
      val rngState = State[RNG, Int](s => s.nextInt)

      val actual = State.sequence(List(rngState, rngState)).run(rng)._1

      actual shouldBe List(random1, random2)
   }
}
