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

   "forAll" should "work" in {
      Stream.apply(1, 2, 3, 4).forAll(_ < 5) shouldBe true
      var c = 0;
      Stream.apply(1, 2, 3, 4).forAll(n => {c = c+1; n == 1}) shouldBe false
      c shouldBe 2
   }

   "foldRight" should "work" in {
      Stream.apply(1, 2, 4).foldRight(0)(_ - _) shouldBe 3
   }

   "takeWhileFR" should "work" in {
      Stream.apply(1, 2, 3, 4, 5, 1, 1, 1).takeWhileFR( _ <= 3).toList shouldBe List(1, 2, 3)
      Stream.apply(1, 2).takeWhileFR(_ <= 4).toList shouldBe List(1, 2)
      Stream.apply(1, 2, 3).takeWhileFR((a) => true).toList shouldBe List(1, 2, 3)
      Stream.apply(1, 2, 3).takeWhileFR((a) => false).toList shouldBe Nil
   }

   "headOptionFR" should "work" in {
      Stream.apply(1, 2, 3).headOptionFR shouldBe Option(1)
      Stream.empty.headOptionFR shouldBe None
   }

   "map" should "work" in {
      Stream.apply(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
      Stream.empty[Int].map(_ + 1).toList shouldBe Nil
   }

   "filter" should "work" in {
      Stream.apply(1, 2, 7, 8, 4, 3).filter(_ % 2 == 0).toList shouldBe List(2, 8, 4)
      Stream.empty[Int].filter(_ % 2 == 0).toList shouldBe Nil
   }

   "append" should "work" in {
      Stream(1, 2).append(Stream(3, 4)).toList shouldBe List(1, 2, 3, 4)
      Stream(1, 2).append(Stream.empty).toList shouldBe List(1, 2)
      Stream.empty.append(Stream(1, 2)).toList shouldBe List(1, 2)
   }

   "flatMap" should "work" in {
      val f = (a: Int) => Stream(-a, a)
      Stream(1, 2, 3).flatMap(f).toList shouldBe List(-1, 1, -2, 2, -3, 3)
      Stream.empty.flatMap(f).toList shouldBe Nil
   }

   "constant" should "work" in {
      Stream.constant(3).take(4).toList shouldBe List(3, 3, 3, 3)
      Stream.constantUnfold(3).take(4).toList shouldBe List(3, 3, 3, 3)
   }

   "from" should "work" in {
      Stream.from(3).take(3).toList shouldBe List(3, 4, 5)
      Stream.fromUnfold(3).take(3).toList shouldBe List(3, 4, 5)
   }

   "fibs" should "work" in {
      Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
      Stream.fibsUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
   }

   "unfold" should "work" in {
      Stream.unfold(1)(s => Option((s, s + 1))).take(4).toList shouldBe List(1, 2, 3, 4)
   }

   "ones" should "work" in {
      Stream.ones.take(3).toList shouldBe List(1, 1, 1)
   }

   "mapUnfold" should "work" in {
      Stream(1, 2, 3).mapUnfold(_ + 1).toList shouldBe List(2, 3, 4)
      Stream.empty[Int].mapUnfold(_ + 1).toList shouldBe Nil
   }

   "takeUnfold" should "work" in {
      Stream(1, 2, 3, 4).takeUnfold(2).toList shouldBe List(1, 2)
      Stream(1, 2, 3, 4).takeUnfold(4).toList shouldBe List(1, 2, 3, 4)
      Stream(1, 2, 3, 4).takeUnfold(5).toList shouldBe List(1, 2, 3, 4)
      Stream(1, 2, 3, 4).takeUnfold(0).toList shouldBe Nil
   }

   "takeWhileUnfold" should "work" in {
      Stream(1, 2, 3, 4, 5, 1, 1, 1).takeWhileUnfold( _ <= 3).toList shouldBe List(1, 2, 3)
      Stream(1, 2).takeWhileUnfold(_ <= 4).toList shouldBe List(1, 2)
      Stream(1, 2, 3).takeWhileUnfold((a) => true).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3).takeWhileUnfold((a) => false).toList shouldBe Nil
   }

   "zipWithUnfold" should "work" in {
      val add = (a: Int, b: Int) => a + b
      Stream(1, 2, 3).zipWithUnfold(Stream(7, 8, 9))(add).toList shouldBe List(8, 10, 12)
      Stream(1, 2, 3).zipWithUnfold(Stream(7))(add).toList shouldBe List(8)
      Stream(1).zipWithUnfold(Stream(7, 8, 9))(add).toList shouldBe List(8)
      Stream().zipWithUnfold(Stream(7, 8, 9))(add).toList shouldBe Nil
      Stream(1, 2, 3).zipWithUnfold(Stream())(add).toList shouldBe Nil
   }


   "zipAllUnfold" should "work" in {
      Stream(1, 2).zipAllUnfold(Stream(7, 8)).toList shouldBe List((Some(1), Some(7)), (Some(2), Some(8)))
      Stream(1, 2).zipAllUnfold(Stream(7)).toList shouldBe List((Some(1), Some(7)), (Some(2), None))
      Stream(1).zipAllUnfold(Stream(7, 8)).toList shouldBe List((Some(1), Some(7)), (None, Some(8)))
      Stream().zipAllUnfold(Stream(7, 8)).toList shouldBe List((None, Some(7)), (None, Some(8)))
      Stream(1, 2).zipAllUnfold(Stream()).toList shouldBe List((Some(1), None), (Some(2), None))
   }

   "startsWith" should "work" in {
      Stream(1, 2, 3, 4).startsWith(Stream(1, 2)) shouldBe true
      Stream(1, 2, 3, 4).startsWith(Stream(2, 3)) shouldBe false
   }


}
