package adrian.handsonscala

//https://www.codewars.com/kata/5536a85b6ed4ee5a78000035/train/scala
object HelpGranny extends App {
//  val friends1 = List("A1", "A2", "A3", "A4", "A5")
//  val fTowns1 = List(("A1", "X1"), ("A2", "X2"), ("A3", "X3"), ("A4", "X4"))
//  val distTable1 = Map("X1" -> 100.0, "X2" -> 200.0, "X3" -> 250.0, "X4" -> 300.0)

  def tour(arrFriends: List[String], ftoT: List[(String, String)], h: Map[String, Double]): Int = {
    val friendsToTown = ftoT.toMap

    val initial = (friendsToTown(arrFriends.head), h(friendsToTown(arrFriends.head)))

    val (lastCityVisited, dTraveled) = arrFriends.tail.foldLeft(initial) {
      case ((lastLocation, dTraveled), nextFriend) =>
        friendsToTown.get(nextFriend)
          .fold(
            (lastLocation, dTraveled)
          )(nextLocation => (nextLocation, dTraveled + length(lastLocation, nextLocation, h)))
    }

    Math.floor(dTraveled + h(lastCityVisited)).toInt
  }

  def length(loc1: String, loc2: String, h:Map[String, Double]): Double = {
    val d1 = h(loc1)
    val d2 = h(loc2)
    Math.sqrt {
      if (d2 > d1)
        (d2 * d2) - (d1 * d1)
      else
        (d1 * d1) - (d2 * d2)
    }
  }
}
