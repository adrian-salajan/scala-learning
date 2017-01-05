package main.chapter6

import main.Test

class TestMachineSim extends Test {

  "machine sim" should "work" in {

    val input = List(Coin, Coin, Coin, Coin, Turn)

    val endState = Simulator.simulateMachine(input).run(Machine(true, 10, 5))

    endState._1 shouldBe (14, 1)

  }

}
