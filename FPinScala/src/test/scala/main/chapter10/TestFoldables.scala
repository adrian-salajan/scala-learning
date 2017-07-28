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

class TestFoldables extends Test{

  val as = List("a", "b", "c")
  val seqAs = IndexedSeq("a", "b", "c")

  "foldableList" should "foldLeft" in {

    import Foldables.forList._
    foldLeft(as)("z")(_ + _) shouldBe "zabc"
    foldLeft(List[String]())("")(_ + _) shouldBe ""

    as.foldLeft("z")(_ + _) shouldBe "zabc"
  }

  it should "foldRight" in {
    import Foldables.forList._
    foldRight(as)("z")(_ + _) shouldBe "abcz"
    foldRight(List[String]())("")(_ + _) shouldBe ""

    as.foldRight("z")(_ + _) shouldBe "abcz"
  }

  it should "foldMap" in {
    import Foldables.forList._
    val ints = List("1", "2", "4")
    foldMap(ints)(_.toInt)(Monoids.intAddition) shouldBe 7
  }

  "foldableSeq" should "foldLeft" in {

    import Foldables.forSeq._
    foldLeft(seqAs)("z")(_ + _) shouldBe "zabc"
    foldLeft(IndexedSeq[String]())("")(_ + _) shouldBe ""

    as.foldLeft("z")(_ + _) shouldBe "zabc"
  }

  it should "foldRight" in {
    import Foldables.forSeq._
    foldRight(seqAs)("z")(_ + _) shouldBe "abcz"
    foldRight(IndexedSeq[String]())("")(_ + _) shouldBe ""

    as.foldRight("z")(_ + _) shouldBe "abcz"
  }

  it should "foldMap" in {
    import Foldables.forSeq._
    val ints = IndexedSeq("1", "2", "4")
    foldMap(ints)(_.toInt)(Monoids.intAddition) shouldBe 7
  }

}
