package main.chapter6

import main.Test

class TestState extends Test {


   "unit" should "work" in {
      State.unit(3).run(SimpleRNG(2)) shouldBe (3, SimpleRNG(2))
   }

//   "map" should "work" in {
//      State.unit(3).map(_ * 2).run(SimpleRNG(2)) shouldBe (6, SimpleRNG(2))
//   }
}
