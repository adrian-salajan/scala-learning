package adrian.codewarriors

//https://www.codewars.com/kata/55e2adece53b4cdcb900006c/train/scala
object TortoiseRacing extends App {
  // when v1 >= v2 return Array(-1,-1,-1)
  def race(v1: Int, v2: Int, g: Int): Array[Int] = {
    if (v1 >= v2)
      Array(-1, -1, -1)
    else {
      val secs =  startRace(v1, v2, g * 3600, 0, 0) //milis / 1000
      val mins = secs / 60
      val hours = mins / 60
      Array(hours, (mins - (hours * 60)) % 60, secs % 60 )
    }
  }

  def startRace(v1: Int, v2: Int, x1: Int, x2: Int, time: Int): Int = {
    if (x2 > x1) time -1
    else startRace(
      v1, v2,
      x1 + v1,
      x2 + v2,
      time + 1)
  }


  println(race(720, 850, 70).mkString("/")) // 0 32 18
  println(race(80, 91, 37).mkString("/"))//3 21 49
  println(race(383, 1183, 142).mkString("/")) // Array(0, 10, 39)


}
