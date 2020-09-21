package adrian.codewarriors

import adrian.codewarriors.SumSquaredDivisorsTest._
import org.scalatest.Assertions._
import org.scalatest.flatspec.AnyFlatSpecLike

class SumSquaredDivisorsTest extends AnyFlatSpecLike {


  it should "pass basic tests" in {

    val start = System.nanoTime()
    dotest(1, 250, "[[1, 1], [42, 2500], [246, 84100]]")
    dotest(42, 250, "[[42, 2500], [246, 84100]]")
    dotest(250, 500, "[[287, 84100]]")
    SumSquaredDivisors.listSquared(327, 4381)
    SumSquaredDivisors.listSquared(708, 7047)
    println("done in ms: " + (System.nanoTime() - start) / 1000000)
  }
}

object SumSquaredDivisorsTest {

  private def dotest(m: Long, n: Long, expect: String): Unit = {
    println("Testing: " + m + ", " + n)
    val actual: String = SumSquaredDivisors.listSquared(m, n)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
}
