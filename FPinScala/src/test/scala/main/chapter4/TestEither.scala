package main.chapter4

import main.Test

class TestEither extends Test {

   "map" should "work" in {
      Right(5) map (_ + 2) shouldBe Right(7)
      Left("error").map( _ => 4) shouldBe Left("error")
   }

   "flatMap" should "work" in {
      def sqrt = (a :Int) => if (a > 0) Right(Math.sqrt(a)) else Left("cannot sqrt negative number")

      Right(9) flatMap sqrt shouldBe Right(3)
      Right(-9).flatMap(sqrt) shouldBe Left("cannot sqrt negative number")
      Left("error") flatMap sqrt shouldBe Left("error")
   }

   "orElse" should "work" in {
      Right(9).orElse(Right(7)) shouldBe Right(9)

      Left().orElse(Right(1)) shouldBe Right(1)
   }

   "map2" should "work" in {
      val add = (a: Int, b: Int) => a + b

      Right(9).map2(Right(1))(add) shouldBe Right(10)
      Right(9).map2(Left())(add) shouldBe Left()
      Left().map2(Right(1))(add) shouldBe Left()
   }

   "sequence" should "work" in {
      Either.sequence(List(Right(1), Right(3))) shouldBe Right(List(1, 3))
      Either.sequence(List(Left("L"), Right(3))) shouldBe Left("L")
      Either.sequence(List(Right(3), Left("L"))) shouldBe Left("L")
   }

   "traverse" should "work" in {
      val f = (s : String) => if (s.matches("[0-9]+")) Right(s.toInt) else Left("string must be digit")
      Either.traverse(List("1", "2"))(f) shouldBe Right(List(1, 2))
      Either.traverse(List("1", "r"))(f) shouldBe Left("string must be digit")
      Either.traverse(List("r", "2"))(f) shouldBe Left("string must be digit")
   }

   "sequence2" should "work" in {
      Either.sequence2(List(Right(1), Right(3))) shouldBe Right(List(1, 3))
      Either.sequence2(List(Left("L"), Right(3))) shouldBe Left("L")
      Either.sequence2(List(Right(3), Left("L"))) shouldBe Left("L")
   }


}
