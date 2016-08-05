package main.chapter2

import main.Test
import main.chapter2.Chapter2.{fib, isSorted}

/**
  * Created by adrian on 3/8/2016.
  */
class TestChapter2 extends Test {

   "Fib()" should "produce fib numbers" in {
      fib(0) shouldBe 0
      fib(10) shouldBe 55
   }

   def cmpInt(a: Int, b: Int) = a <= b;
   def cmpStrLen(a: String, b: String) = a.length <= b.length;

   "IsSorted()" should "return true" in {
      isSorted(Array(1, 2, 3), cmpInt) shouldBe true
      isSorted(Array(-1, 2, 5), cmpInt) shouldBe true
      isSorted(Array(2, 3, 3), cmpInt) shouldBe true
      isSorted(Array(2), cmpInt) shouldBe true

      isSorted(Array("", "a", "aa"), cmpStrLen) shouldBe true
   }

   it should "return false" in {
      isSorted(Array(3, 2, 1), cmpInt) shouldBe false
      isSorted(Array(3, 2, 4), cmpInt) shouldBe false
      isSorted(Array(-9, -11), cmpInt) shouldBe false

      isSorted(Array("aa", "a", "aaa"), cmpStrLen) shouldBe false
   }

   val sum = (a: Int, b: Int) => a + b
   "curry" should "curry functions" in {
      val curriedSum = Chapter2.curry(sum)
      val add2 = curriedSum(2)

      add2(3) shouldBe 5
      add2(-3) shouldBe -1
   }

   "uncurry" should "uncurry functions" in {
      val curriedSum = Chapter2.curry(sum)

      val uncuried = Chapter2.uncurry(curriedSum)

      uncuried(2, 3) shouldBe 5

   }

   "compose" should "compose functions" in {
      val plus1 = (a: Int) => a + 1
      val square = (a: Int) => a * a

      val squareAdd1 =  Chapter2.compose(plus1, square)

      squareAdd1(2) shouldBe 5
      (plus1 compose square)(2) shouldBe 5
      val add1Squared =  Chapter2.compose(square, plus1)

      add1Squared(2) shouldBe 9
      (square compose plus1)(2) shouldBe 9
   }

   "test" should "test" in {

   }
}

