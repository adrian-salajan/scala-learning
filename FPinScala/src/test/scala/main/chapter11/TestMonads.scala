package main.chapter11

import main.Test

class TestMonads extends Test {

  "replicateM" should "work for lists" in {

    val ml = Monads.forList.unit(3)

    Monads.forList.replicateM(2, ml) shouldBe List(
      List(3, 3)
    )

  }

  it should "work for option" in {
    val s = Monads.forOption.unit(3)

    Monads.forOption.replicateM(2, s) shouldBe Option(List(3, 3))
  }

  it should "work for State" in {
    val stateMonad = Monads.stateMonad[Unit]
    val s = stateMonad.unit("x")

    stateMonad.replicateM(2, s).run(())._1 shouldBe List("x", "x")
  }

  "filterM" should "work" in {
    val li = List(1, 2, 3)
    Monads.forOption.filterM(li)(e => if (e >= 2) Option(true) else Option(false)) shouldBe Monads.forOption.unit(List(2, 3))

    Monads.forList.filterM(li)(e => if (e >=2 ) List(true) else List(false)) shouldBe Monads.forList.unit(List(2, 3))
  }




}
