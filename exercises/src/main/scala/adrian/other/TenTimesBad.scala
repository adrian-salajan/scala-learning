package adrian.other

import adrian.other.EqualsUnbroken.ValueFromEffect

object TenTimesBad {

  def main(args: Array[String]): Unit = {
  def assertToConsole(b: Boolean): Unit = println(s"assert is $b")
  val result =
    readNumberValueDoubled.flatMap(a =>
      readNumberValueDoubled.flatMap(b => new ValueFromEffect(() => a == b)))
      .runEffectToGetValue

  val printOnce = assertToConsole(result)
  List.fill(10)(printOnce) //prints "assert is X" only once!
  }

  val readNumberValue = new ValueFromEffect(() => Console.in.readLine().toInt)
  val readNumberValueDoubled = readNumberValue.flatMap(a => readNumberValue.flatMap(b => new ValueFromEffect(() => a + b)))
}
