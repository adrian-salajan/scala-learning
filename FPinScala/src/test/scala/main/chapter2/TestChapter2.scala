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
}

