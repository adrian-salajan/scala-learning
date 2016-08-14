package main.chapter3

import main.Test
import main.chapter3.MyList._
import main.chapter3.{Cons, MyList, Nili}

class TestMyList extends Test {

  "myList" should "behave as a linked list" in {
    val list = Cons(1, Cons(2, Cons(3, Nili)))
    val list2 = MyList(1, 2, 3)

    list shouldBe list2
  }

  "sum" should "sum int list" in {
    MyList.sum(MyList(1, 4, 5)) shouldBe 10
  }

  "fill" should "build a filled list" in {
    fill(3, "a") shouldBe MyList("a", "a", "a")
    fill(0, "a") shouldBe Nili
  }

  "tail" should "return tail" in {
    tail(MyList(1)) shouldBe Nili
    tail(MyList(1,2)) shouldBe MyList(2)
  }

  it should "throw exception for Nili" in {
    a [UnsupportedOperationException] should be thrownBy {
      tail(Nili)
    }
  }

  "replaceHead" should "replace head of list" in {
    MyList.replaceHead(10, Nili) shouldBe MyList(10)
    MyList.replaceHead(10, MyList(1, 2, 3)) shouldBe MyList(10, 2 ,3)
  }

  "drop" should "remove the first n elements" in {
    drop(0, Nili) shouldBe Nili
    drop(1, MyList(1)) shouldBe Nili
    drop(0, MyList(2, 3)) shouldBe MyList(2, 3)
    drop(1, MyList(2, 3)) shouldBe MyList(3)
    drop(2, MyList(1, 2, 3, 4)) shouldBe MyList(3, 4)
    drop(-2, MyList(1, 2, 3, 4)) shouldBe MyList(1, 2, 3, 4)
  }

  it should "throw exception for Nili" in {
    a [UnsupportedOperationException] shouldBe thrownBy {
      drop(1, Nili)
    }
  }

  "dropWhile" should "drop elements till false" in {
    dropWhile(MyList("a", "ab", "abc", "b", "bc"))(s => s.startsWith("a")) shouldBe MyList("b", "bc")
  }

  "init" should "drop last element" in {
    init(MyList(1, 2, 3)) shouldBe MyList(1, 2)
    init(MyList(1)) shouldBe Nili
    init(Nili) shouldBe Nili
  }

  "foldRight" should "foldRight" in {
    val l = MyList(1, 2, 3)
    foldRight(l, 0)(_ + _) shouldBe 6
    foldRight(l, Nili:MyList[Int])(Cons(_, _)) shouldBe l

    foldRight(MyList(2, 3, 4), 0)(_ - _) shouldBe 3
  }

  it should "throw stack overflow exception" in {
    var l = MyList(1)
    (1 to 1000000).foreach { n => l = Cons(1, l)}
    a [StackOverflowError] should be thrownBy {
      foldRight(l, 0)(_ + _)
    }
  }

  "length" should "compute length" in {
    MyList.length(MyList(1, 2, 3)) shouldBe 3
    MyList.length(MyList(1)) shouldBe 1
    MyList.length(MyList()) shouldBe 0
  }

  "foldLeft" should "foldLeft" in {
    foldLeft(MyList(2, 3, 4), 0)(_ - _) shouldBe -9
  }

  "foldLeft" should "sum without stack overflow" in {
    var l = MyList(1)
    (1 to 1000000).foreach { n => l = Cons(1, l)}
    MyList.foldLeft(l, 0)(_ + _) shouldBe 1000001
  }

  "sum2" should "sum" in {
    sum2(MyList(1, 2, 4)) shouldBe 7
    sum2(MyList()) shouldBe 0
  }

  "product2" should "multiply" in {
    product2(MyList(2, 2, 3)) shouldBe 12
    product2(MyList()) shouldBe 1
  }

  var theList = MyList(2, 3, 4)

  "length2" should "return length" in {
    length2(theList) shouldBe 3
    length2(Nili) shouldBe 0
  }

  "reverse" should "reverse list" in {
    reverse(theList) shouldBe MyList(4, 3, 2)
  }

  "foldLeftViaFoldRight" should "foldLeftViaFoldRight" in {
    foldLeftViaFoldRight(theList, 0)(_ - _) shouldBe -9
  }

  "foldRightViaFoldLeft" should "foldRightViaFoldLeft" in {
    foldRightViaFoldLeft(theList, 0)(_ - _) shouldBe 3
  }

  "append" should "add element at end" in {
    append(4, MyList(1, 2, 3)) shouldBe MyList(1, 2, 3, 4)
    append("a", MyList()) shouldBe MyList("a")
  }

  "flatten" should "flatten" in {
    flatten(MyList(MyList(1, 2), MyList(3, 4), MyList(5, 6))) shouldBe MyList(1, 2, 3, 4, 5, 6)
  }

  "incrementAllBy1" should "add 1 to all" in {
    incrementAllBy1(MyList(-1, 2, 0)) shouldBe MyList(0, 3, 1)
  }

  "doublesToStrings" should "convert doubles to strings" in {
    doublesToStrings(MyList(2d, 3.0d, 4.01d)) shouldBe MyList("2.0", "3.0", "4.01")
  }

  "filter" should "remove elements that dont match predicate" in {
    filter(MyList(2, 3, 4, 5))(_ % 2 == 0) shouldBe MyList(2, 4)
  }

  "map" should "transform each element" in {
    map(MyList(1, 2, 3))(_.toString) shouldBe MyList("1", "2", "3")
  }

  "concat" should "concat 2 lists" in {
    concat(MyList(1, 2), MyList(3, 4)) shouldBe MyList(1, 2, 3, 4)
    concat(Nili, MyList(3, 4)) shouldBe MyList(3, 4)
    concat(MyList(1, 2), Nili) shouldBe MyList(1, 2)
  }

  def duplicate[A](a: A) = MyList(a, a)

  "flatMap" should "flat map" in {
    flatMap(MyList(1, 2, 3))(duplicate) shouldBe MyList(1, 1, 2, 2, 3, 3)
    flatMap(MyList())(duplicate) shouldBe MyList()
  }

  "addLists" should "add lists" in {
    addLists(MyList(1, 2, 3), MyList(4, 5, 6)) shouldBe MyList(5, 7, 9)

    addLists2(MyList(1, 2, 3), MyList(4, 5, 6)) shouldBe MyList(5, 7, 9)
  }

  "zipWith" should "zip with f" in {
    zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ + _) shouldBe MyList(5, 7, 9)
  }

  "hasSubsequence" should "return true for valid subsequence" in {
    hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 3)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(3, 4)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(1)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(2)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(4)) shouldBe true
    hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2, 3, 4)) shouldBe true

    hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 3)) shouldBe false
    hasSubsequence(MyList(1, 2, 3, 4), MyList(4, 5)) shouldBe false
    hasSubsequence(MyList(1, 2, 3, 4), MyList(7)) shouldBe false
    hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2, 3, 4, 5)) shouldBe false
  }
}


