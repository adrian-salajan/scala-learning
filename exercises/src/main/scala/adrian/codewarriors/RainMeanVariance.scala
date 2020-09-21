package adrian.codewarriors

//https://www.codewars.com/kata/55e2adece53b4cdcb900006c/train/scala
object RainMeanVariance extends App {

  def mean(town: String, strng: String): Double = {
   parsed(strng).get(town).map(_.sum / 12).getOrElse(-1)

  }

  def variance(town: String, strng: String): Double = {
    parsed(strng).get(town).map { months =>
      val mean = months.sum / 12
      val diff = months.view.map(a =>a - mean).map(a => a * a)
      val variance = diff.sum / 12
      variance
    }.getOrElse(-1)
  }


  def dataCity(line: String): Array[Double] = {
   line.split(",")
     .map(_.trim)
     .map(month => month.split(" ")(1).toDouble)

  }
  def parsed(data: String): Map[String, Array[Double]] =
    data.split("\n").map { line =>
    val Array(city, data) = line.split(":")
    (city, dataCity(data))
  }.toMap

  println(mean("London", "London:Jan 48.0,Feb 38.9,Mar 39.9,Apr 42.2,May 47.3,Jun 52.1,Jul 59.5, Aug 57.2,Sep 55.4,Oct 62.0,Nov 59.0,Dec 52.9"))
  println(variance("London", "London:Jan 48.0,Feb 38.9,Mar 39.9,Apr 42.2,May 47.3,Jun 52.1,Jul 59.5, Aug 57.2,Sep 55.4,Oct 62.0,Nov 59.0,Dec 52.9" ))

  //"London:Jan 48.0,Feb 38.9,Mar 39.9,Apr 42.2,May 47.3,Jun 52.1,Jul 59.5, Aug 57.2,Sep 55.4,Oct 62.0,Nov 59.0,Dec 52.9


}
