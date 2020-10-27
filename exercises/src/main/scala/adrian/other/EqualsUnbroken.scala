package adrian.other

object EqualsUnbroken {

  class ValueFromEffect[A](a : () => A) {
    def runEffectToGetValue: A = a()

    def flatMap[B](f: A => ValueFromEffect[B]): ValueFromEffect[B] =
      new ValueFromEffect[B](() => f(runEffectToGetValue).runEffectToGetValue)

    //def map[B](f: A => B): ValueFromEffect[B] = flatMap(v => new ValueFromEffect(() => f(v)))
  }
  object ValueFromEffect {
    def suspend[A](a: => A)  = new ValueFromEffect[A](() => a)
  }

  def main(args: Array[String]): Unit = {

//    val five = 5
//
//    val fiveDoubled = five + five
//    val fiveDoubledInline = 5 + 5
//
//    assert(fiveDoubled == fiveDoubledInline)

    //####

//    val readNumber = Console.in.readLine().toInt
//
//    val readNumberPlusReadNumber = readNumber + readNumber
//    val readNumberPlusReadNumberInlined = Console.in.readLine().toInt + Console.in.readLine().toInt
//
//    assert(readNumberPlusReadNumber == readNumberPlusReadNumberInlined)
//println("done")
    //###



//    object ValueFromEffect {
//      def alwaysAValue[A](a: A) =
//    }

    //    val readNumberValueDoubled = readNumberValue + readNumberValue
    val readNumberValue = new ValueFromEffect(() => Console.in.readLine().toInt)
    val readNumberValueDoubled = readNumberValue.flatMap(a => readNumberValue.flatMap(b => new ValueFromEffect(() => a + b)))
    val readNumberValueDoubledInlined =  new ValueFromEffect(() => Console.in.readLine().toInt).flatMap(a => new ValueFromEffect(() => Console.in.readLine().toInt).flatMap(b => new ValueFromEffect(() => a + b)))


//    def assertToConsole(b: Boolean): Unit = println(s"assert is $b")
//    val assertResult = readNumberValueDoubled.flatMap(a =>
//      readNumberValueDoubledInlined.flatMap(b => new ValueFromEffect(() => a == b))
//    ).runEffectToGetValue
//    val printOnce = assertToConsole(assertResult)
//    List.fill(10)(printOnce)

//    def assertToConsole(b: Boolean): Unit = println(s"assert is $b")
//    val assertResult = readNumberValueDoubled.flatMap(a =>
//      readNumberValueDoubledInlined.flatMap(b => new ValueFromEffect(() => a == b))
//    ).runEffectToGetValue
//    val printOnce = new ValueFromEffect(() => assertToConsole(assertResult))
//    val printTenTimes = List.fill(10)(printOnce).reduce ( (e1, e2) => e1.flatMap(_ => e2) )
//    printTenTimes.runEffectToGetValue

    def assertToConsole(b: Boolean): Unit = println(s"assert is $b")
    val assertResult = readNumberValueDoubled.flatMap(a =>
      readNumberValueDoubledInlined.flatMap(b => new ValueFromEffect(() => a == b))
    )
    val printTenTimes = assertResult.flatMap(r =>
      List.fill(10)(new ValueFromEffect(() => assertToConsole(r))).reduce ( (e1, e2) => e1.flatMap(_ => e2))
    )
    printTenTimes.runEffectToGetValue



//    assert(readNumberValueDoubled.runEffectToGetValue == readNumberValueDoubledInlined.runEffectToGetValue)
//    assertToConsole(readNumberValueDoubled.runEffectToGetValue == readNumberValueDoubledInlined.runEffectToGetValue)
//    assertToConsole(readNumberValueDoubled.runEffectToGetValue == readNumberValueDoubledInlined.runEffectToGetValue)

//    val assertEffect: ValueFromEffect[Unit] = readNumberValueDoubled.flatMap(a => readNumberValueDoubledInlined.flatMap(
//      b => new ValueFromEffect[Unit](() => assertToConsole(a == b))
//    ))
//    assertEffect.runEffectToGetValue

//    val runTwice = assertEffect.flatMap(_ => assertEffect)
//    runTwice.runEffectToGetValue
//    val program = readNumberValueDoubled.flatMap(a => readNumberValueDoubledInlined.flatMap(b => new ValueFromEffect[Unit](() => assert(a == b))))
//
//    program.runEffectToGetValue


    ////











//    val printFive = println(5)
//
//    val printFiveTwice = printFive + printFive
//    val numberPlusNumberInlined = Console.in.readLine().toInt + Console.in.readLine().toInt
//
//    print("assert:")
//    assert(numberPlusNumber == numberPlusNumberInlined)




    println("done2")

  }
}
