package adrian.codewarriors

object TriangleSumLevel {
  def rowSumOddNumbers(n: Long): Long = {
    val index = List.tabulate(n.toInt)(identity).sum
    val m = (index * 2) - 1
    List.tabulate(n.toInt)(a => m + (2 * (a + 1))).sum

  }

  def nbYear(initialPop: Int, percent: Double, extra: Int, finalPop: Int): Int = {
      if (initialPop >= finalPop) 0
      else nbYear(initialPop + (initialPop * percent/100).toInt + extra, percent, extra, finalPop)
  }






  def main(a: Array[String]): Unit = {
    println(nbYear(1500000, 0, 10000, 2000000))
  }

}
