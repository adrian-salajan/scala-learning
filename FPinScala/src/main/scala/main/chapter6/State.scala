package main.chapter6

case class State[S,+A](run: S => (A,S))
{
//   def map[B](f: A => B): State[S, B] = {
//      State((s) => {
//         val (a, s2) = run(s)
//         (f(a), s2)
//      })
//   }

   def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      state => {
         val (a, state2) = run(state)
         f(a).run(state2)
      }
   )
}

//unit, map, map2, flatMap, and sequence.
object State {
   def unit[S, A](a: A):State[S, A] = State((s) => (a, s))
}
