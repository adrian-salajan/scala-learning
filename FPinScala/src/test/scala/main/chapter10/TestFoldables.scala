package main.chapter10

import main.Test
import main.chapter3.{Leaf, Node}

class TestFoldables extends Test{

  val as = List("a", "b", "c")
  val seqAs = IndexedSeq("a", "b", "c")
  val streamAs = Stream("a", "b", "c")

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

  "foldableStream" should "foldLeft" in {

    import Foldables.forStream._
    foldLeft(streamAs)("z")(_ + _) shouldBe "zabc"
    foldLeft(Stream[String]())("")(_ + _) shouldBe ""

    streamAs.foldLeft("")(_ + _) shouldBe "abc"
  }

  it should "foldRight" in {
    import Foldables.forStream._
    foldRight(streamAs)("z")(_ + _) shouldBe "abcz"
    foldRight(Stream[String]())("")(_ + _) shouldBe ""

    streamAs.foldRight("z")(_ + _) shouldBe "abcz"
  }

  it should "foldMap" in {
    import Foldables.forStream._
    val ints = Stream("1", "2", "4")
    foldMap(ints)(_.toInt)(Monoids.intAddition) shouldBe 7
  }


  val treeAs = Node(
        Leaf("a"), Node(
                  Leaf("b"), Leaf("c"))
  )

  "foldableTree" should "foldLeft" in {

    import Foldables.forTree._
    foldLeft(treeAs)("z")(_ + _) shouldBe "zabc"
  }

  "foldableTree" should "foldRight" in {

    import Foldables.forTree._
    foldRight(treeAs)("z")(_ + _) shouldBe "abcz"
  }

  "toList" should "work for every foldable" in {
    Foldables.forList.toList(as) shouldBe as
    Foldables.forSeq.toList(seqAs) shouldBe as
    Foldables.forStream.toList(streamAs) shouldBe as
    Foldables.forTree.toList(treeAs) shouldBe as
    Foldables.forOption.toList(Option.empty) shouldBe Nil
    Foldables.forOption.toList(Option(3)) shouldBe List(3)
  }

}
