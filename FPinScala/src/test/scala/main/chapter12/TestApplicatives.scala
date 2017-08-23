package main.chapter12

import main.Test

class TestApplicatives extends Test {

  "applicativeR map2" should "work" in {
    val fa = Applicatives.listApplic.unit(2)
    val fb = Applicatives.listApplic.unit(4)

    Applicatives.listApplic.map2(fa, fb)( (a, b) => a + b) shouldBe List(6)
  }

  "sequenceMap" should "work" in {
    val m = Map(1 -> Option("1"), 2 -> Option("2"))

    Applicatives.optionApplic.sequenceMap(m) shouldBe Option(Map(1 -> "1", 2 -> "2"))
  }

  "product" should "work" in {
    val product = Applicatives.listApplic.product(Applicatives.optionApplic)
    val one = product.unit(1)

    one shouldBe (List(1), Option(1))
    def inc = (x: Int) => x + 1
    product.map(one)(inc) shouldBe (List(2), Option(2))
  }

}
