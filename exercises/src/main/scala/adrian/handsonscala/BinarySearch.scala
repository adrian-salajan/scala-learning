package adrian.handsonscala


object BinarySearch extends App {

  def search(ints: Array[Int], n: Int, a: Int, b: Int): Boolean =
    if (a == b) {
      if (ints(a) == n) true else false
    } else {
      val half = a + (b - a) / 2
      if (n <= ints(half))
        search(ints, n, a, half)
      else
        search(ints, n, half + 1, b)
    }

  def binarySearch(ints: Array[Int], n: Int): Boolean = {
    if (ints.isEmpty) false
    else {
      search(ints, n, 0, ints.length-1)
    }
  }


  private val ints: Array[Int] = Array(2, 3, 4, 6, 7, 8, 9, 10, 11)

  assert(binarySearch(ints, 2))
  assert(binarySearch(ints, 7))
  assert(binarySearch(ints, 11))

  assert(!binarySearch(ints, 1))
  assert(!binarySearch(ints, 5))
  assert(!binarySearch(ints, 12))


  assert(binarySearch(Array(1), 1), true)
  assert(!binarySearch(Array(1), 3))

  println("done")


}
