package adrian.other

import adrian.other.EqualsUnbroken.ValueFromEffect

object PureToImpure {

  def main(args: Array[String]): Unit =
    pureProgram.runEffectToGetValue

  def assertToConsole(b: Boolean): Unit = println(s"assert is $b")

  def pureProgram: ValueFromEffect[Unit] = {

    val result = readNumberValueDoubled.flatMap(a =>
      readNumberValueDoubled.flatMap(b =>
        new ValueFromEffect(() => a == b)))

    def printOnce(b: Boolean) = new ValueFromEffect(() => assertToConsole(b))

    val printTenTimes = result.flatMap(r =>
      List.fill(10)(printOnce(r)).reduce ( (e1, e2) => e1.flatMap(_ => e2))
    )

    printTenTimes
  }

  val readNumberValue = new ValueFromEffect(() => Console.in.readLine().toInt)
  val readNumberValueDoubled = readNumberValue.flatMap(a => readNumberValue.flatMap(b => new ValueFromEffect(() => a + b)))
}
