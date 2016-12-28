package main.chapter5

import main.Test

class TestStream extends Test {

   "toList" should "work" in {
      Stream.apply(1 ,2 ,3).toList shouldBe List(1, 2, 3)
   }

   "take" should "work" in {
      Stream.apply(1, 2, 3, 4, 5).take(3).toList shouldBe List(1, 2, 3)
      Stream.apply(1, 2).take(3).toList shouldBe List(1, 2)
      Stream.apply(1, 2).take(0) shouldBe Stream.empty
      Stream.empty.take(3) shouldBe Stream.empty
   }

   "drop" should "work" in {
      Stream.apply(1, 2, 3, 4, 5).drop(3).toList shouldBe List(4, 5)
      Stream.apply(1, 2).drop(3) shouldBe Stream.empty
      Stream.empty.drop(3) shouldBe Stream.empty
   }

   "takeWhile" should "work" in {
      Stream.apply(1, 2, 3, 4, 5, 1, 1, 1).takeWhile( _ <= 3).toList shouldBe List(1, 2, 3)
      Stream.apply(1, 2).takeWhile(_ <= 4).toList shouldBe List(1, 2)
      Stream.apply(1, 2, 3).takeWhile((a) => true).toList shouldBe List(1, 2, 3)
      Stream.apply(1, 2, 3).takeWhile((a) => false).toList shouldBe Nil
   }

}
