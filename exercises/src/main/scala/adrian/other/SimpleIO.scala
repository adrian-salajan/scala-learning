package adrian.other


object SimpleIO extends App {

  class ValueFromEffectSimple[A](a : () => A) {
    def runEffectToGetValue: A = a()

    def flatMap[B](f: A => ValueFromEffectSimple[B]): ValueFromEffectSimple[B] =
      new ValueFromEffectSimple[B](() => f(runEffectToGetValue).runEffectToGetValue)
  }


  val a = new ValueFromEffectSimple[Int](() => Console.in.readLine().toInt)
  println("defined A")
  val x = a.flatMap(x=> a.flatMap(y => a.flatMap(z => new ValueFromEffectSimple(() => x + y + z))))
  println("defined X")

  println("begin")
  x.runEffectToGetValue
  println("end")

}
