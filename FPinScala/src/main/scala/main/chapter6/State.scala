package main.chapter6

case class State[S,+A](run: S => (A,S))
{
//   def map[B](f: A => B):State[S, B] = State(s => {
//      val (a, s2) = run(s)
//      (f(a), s2)
//   })

   def map[B](f: A => B):State[S, B] = flatMap(x => State.unit(f(x)))

//   def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
//      val (a, state1) = run(s)
//      val (b, state2) = s2.run(state1)
//      (f(a, b), state2)
//   })

   def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => s2.map(b => f(a,b)))

   def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, state1) = run(s)
      f(a).run(state1)
   })

}

//unit, map, map2, flatMap, and sequence.
object State {
   def unit[S, A](a: A):State[S, A] = State((s) => (a, s))

   def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
      val z = State.unit[S, List[A]](List[A]())
      states.reverse.foldLeft(z)((acc, nextState) => nextState.map2(acc)((i, ac) => i :: ac))
   }
}
