package main.chapter3

import main.Test
import main.chapter3.{Cons, MyList, Nili}

class TestChapter3 extends Test {

  "myList" should "behave as a linked list" in {
    val list = Cons(1, Cons(2, Cons(3, Nili)))
    val list2 = MyList(1, 2, 3)

    MyList.sum(list) shouldBe 6
    MyList.sum(list2) shouldBe 6

    list shouldBe list2
  }

  "myList.fill" should "build a filled list" in {
    MyList.fill(3, "a") shouldBe MyList("a", "a", "a")
    MyList.fill(0, "a") shouldBe Nili
  }
}
