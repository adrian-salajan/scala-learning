package main.chapter6
import State._

case class Machine(locked: Boolean, coins: Int, candies: Int)

object Simulator {

//   Inserting a coin into a locked machine will cause it to unlock if there’s any
//    candy left.
//   Turning the knob on an unlocked machine will cause it to dispense candy and
//    become locked.
//   Turning the knob on a locked machine or inserting a coin into an unlocked
//    machine does nothing.
//   A machine that’s out of candy ignores all inputs.


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val LOCKED = true
    val UNLOCKED = false

    val states =
      inputs.map(
        in => modify[Machine](
          m => (in, m) match {
            case (Coin, Machine(LOCKED, a, b)) => if (b >0) Machine(UNLOCKED, a + 1, b - 1) else m
            case (Coin, Machine(UNLOCKED, a, b)) => m
            case (Turn, Machine(UNLOCKED, a, b)) => Machine(LOCKED, a, b)
            case (Turn, Machine(LOCKED, a, b)) => m
            case (_, Machine(_, _, 0)) => m
          }
        )
      )

    for {
      r <- get
      _ <- sequence(states)
    } yield (r.coins, r.candies)

  }



}
