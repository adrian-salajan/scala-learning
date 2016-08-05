package main.chapter3

import main.Test
import main.chapter3.MyList.{drop, fill, tail}
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
}
