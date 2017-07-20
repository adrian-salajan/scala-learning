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

import main.Test
import main.chapter6.{RNG, SimpleRNG}
import main.chapter8.Gen
import main.chapter8.Prop.Passed

class TestMonoids extends Test {

  "optionMonoid" should "work" in {

    import Monoids.optionMonoid

    val zero = optionMonoid.zero

    //identity
    optionMonoid.op(zero, Some(3)) shouldBe Some(3)
    optionMonoid.op(Some(3), zero) shouldBe Some(3)

    //assoc
    //  ((a+b)+c)
    optionMonoid.op(optionMonoid.op(Some(3), zero), Some(4)) shouldBe Some(3)
    //  (a+(b+c))
    optionMonoid.op(Some(3), optionMonoid.op(zero, Some(4))) shouldBe Some(3)

    //  ((a+b)+c)
    optionMonoid.op(optionMonoid.op(zero, Some(3)), zero) shouldBe Some(3)
    //  (a+(b+c))
    optionMonoid.op(zero, optionMonoid.op(Some(3), zero)) shouldBe Some(3)
  }

  "endoMonoid" should "work" in {
    import Monoids.endoMonoid
    val zero = endoMonoid[Int].zero

    val increment = (x: Int) => x + 1
    val increment2 = (x: Int) => x + 2

    //identity
    endoMonoid.op(zero, increment)(2) shouldBe 3
    endoMonoid.op(increment, zero)(2) shouldBe 3

    endoMonoid.op(endoMonoid.op(increment2, zero), increment)(2) shouldBe 5
    endoMonoid.op(increment2, endoMonoid.op(zero, increment))(2) shouldBe 5
  }

  "monoidLaws" should "work" in {

    Monoids.monoidLaws(Monoids.intAddition, Gen.choose(1, 100)).run(20, SimpleRNG(2601)) shouldBe Passed

    Monoids.monoidLaws(Monoids.booleanAnd, Gen.boolean).run(20, SimpleRNG(3331)) shouldBe Passed

    def optionIntGen: Gen[Option[Int]] = Gen.choose(0, 100).flatMap(x => if (x % 2 == 0) Gen.unit(Option(x)) else Gen.unit(None))
    Monoids.monoidLaws(Monoids.optionMonoid[Int], optionIntGen).run(20, SimpleRNG(7017)) shouldBe Passed

    Monoids.monoidLaws2(Monoids.intAddition, Gen.choose(1, 100)).run(20, SimpleRNG(2601)) shouldBe Passed
    Monoids.monoidLaws2(Monoids.booleanAnd, Gen.boolean).run(20, SimpleRNG(3331)) shouldBe Passed
    Monoids.monoidLaws2(Monoids.optionMonoid[Int], optionIntGen).run(20, SimpleRNG(7017)) shouldBe Passed

  }
}
