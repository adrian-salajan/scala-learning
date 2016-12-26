package main.chapter4

import java.lang.Exception

import main.Test

import scala.util.Try

class TestOption extends Test {

   "map" should "work" in {
      Option.from(3).map( _ + 2 ) shouldBe Some(5)
      Option.empty.map(a => a) shouldBe None
   }

   "getOrElse" should "work" in {
      Option.from(3).getOrElse(0) shouldBe 3
      Option.empty.getOrElse(-1)shouldBe -1
   }

   "flatMap" should "work" in {
      def sqrt = (a :Int) => if (a > 0) Option.from(Math.sqrt(a)) else None

      Option.from(9).flatMap(sqrt) shouldBe Some(3)
      Option.from(-9).flatMap(sqrt) shouldBe None
   }

   "orElse" should "work " in {
      Option.from(3).orElse(Option.from(-1)) shouldBe Some(3)
      Option.empty.orElse(Option.from(-1)) shouldBe Some(-1)

   }

   "filter" should "work" in {
      Option.from(4).filter(_ % 2 == 0) shouldBe Some(4)
      Option.from(3).filter(_ % 2 == 0) shouldBe None
   }

   //population variance in Wolfram Alpha
   "variance" should "work" in {
      Other.variance(Seq(1, 1, 4)) shouldBe Some(2)
      Other.variance(Seq()) shouldBe None
   }

   "map2" should "work" in {
      val add = (a: Int, b: Int) => a + b
      Option.map2(Some(2), Some(3), add) shouldBe Some(5)
      Option.map2(Some(2), None, add) shouldBe None
      Option.map2(None, Some(3), add) shouldBe None
   }

   "sequence" should "work" in {
      val list = List(Some(3), Some(2), Some(5))
      Option.sequence(list) shouldBe Some(List(3, 2, 5))

      Option.sequence(List(None, Some(3), Some(2))) shouldBe None
      Option.sequence(List(Some(3), None, Some(2))) shouldBe None
      Option.sequence(List(Some(3), Some(2), None)) shouldBe None
   }

   "traverse" should "work" in {
      val toInt: String => Option[Int] = (x:String) =>  {
         try { Some(x.toInt) }
         catch {
            case e: NumberFormatException => None
         }
      }
      val list = List("3", "2", "5")
      Option.traverse(list)(toInt) shouldBe Some(List(3, 2, 5))

      Option.traverse(List("x", "3", "2"))(toInt) shouldBe None
      Option.traverse(List("3", "x", "2"))(toInt) shouldBe None
      Option.traverse(List("3", "2", "x"))(toInt) shouldBe None
   }
   "sequenceT" should "work" in {
      val list = List(Some(3), Some(2), Some(5))
      Option.sequenceT(list) shouldBe Some(List(3, 2, 5))

      Option.sequenceT(List(None, Some(3), Some(2))) shouldBe None
      Option.sequenceT(List(Some(3), None, Some(2))) shouldBe None
      Option.sequenceT(List(Some(3), Some(2), None)) shouldBe None
   }


}
