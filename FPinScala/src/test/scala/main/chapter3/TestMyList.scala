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

  "foldLeft2" should "foldLeft2" in {
    MyList.foldLeft(theList, 0)(_ - _) shouldBe -9
  }


}
