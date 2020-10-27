package adrian.other

import adrian.other.EqualsUnbroken.ValueFromEffect

object TenTimesGood {

  def main(args: Array[String]): Unit = {
  def assertToConsole(b: Boolean): Unit = println(s"assert is $b")
    val result =
      readNumberValueDoubled.flatMap(a =>
        readNumberValueDoubled.flatMap(b => new ValueFromEffect(() => a == b)))
        .runEffectToGetValue

    val printOnce = new ValueFromEffect(() => assertToConsole(result))
    val printTenTimes = List.fill(10)(printOnce).reduce ( (e1, e2) => e1.flatMap(_ => e2) )
    printTenTimes.runEffectToGetValue //prints "assert is X" 10 times
  }

  val readNumberValue = new ValueFromEffect(() => Console.in.readLine().toInt)
  val readNumberValueDoubled = readNumberValue.flatMap(a => readNumberValue.flatMap(b => new ValueFromEffect(() => a + b)))
}
