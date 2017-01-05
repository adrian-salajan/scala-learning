package main.chapter6

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

    val init: State[Machine, (Int, Int)] = State(m => ((m.coins, m.candies), m))

    val LOCKED = true
    val UNLOCKED = false

    inputs.foldLeft(init)((m, i) => {
      i match {
        case Coin =>
          m.get.flatMap(s => s match {
            case Machine(LOCKED, co, ca) =>
              if (ca > 0) m.set(Machine(UNLOCKED, co + 1, ca - 1))
              else m
            case Machine(UNLOCKED, co, ca) => m
            case Machine(_, co, 0) => m
          })
        case Turn =>
          m.get.flatMap(s => s match {
            case Machine(UNLOCKED, co, ca) => m.set(Machine(LOCKED, co, ca))
            case Machine(LOCKED, co, ca) => m
            case Machine(_, co, 0) => m
          })
      }
    })

  }


}
