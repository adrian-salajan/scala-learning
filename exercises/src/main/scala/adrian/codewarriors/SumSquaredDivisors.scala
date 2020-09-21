package adrian.codewarriors

//https://www.codewars.com/kata/55aa075506463dac6600010d/train/scala
object SumSquaredDivisors {


  import scala.collection.mutable

  def listSquared(m: Long, n: Long): String = {
"a"
//    val (_, ns) = (m to n).foldLeft(
//      (mutable.Map[Long, Seq[Long]](), mutable.SortedMap[Long, Long]())
//    ) {
//      case ((divs, found), i) =>
//
//        val divsI = divisors(i)
//        val sum = divsI.map(a => a * a).sum
//
//        val sq = Math.sqrt(sum)
//        val (divs2, found2)= if ((sq - sq.toInt) == 0)
//          (divs + (i -> divsI), found + (i -> sum)) else
//          (divs + (i -> divsI), found)
//
//
//
//    }
//
//   "[" + ns.map {
//      case (a, b) => s"[$a, $b]"
//    }.mkString(", ") + "]"

  }


  def divisors(x: Long): Vector[Long] = {
    val r = for {
      c <- (1L to Math.sqrt(x).toInt).toVector
      if (x % c)==0
    }
    yield if (c == x/c) Vector(c) else Vector(c, x/c)
    r.flatten
  }


}
