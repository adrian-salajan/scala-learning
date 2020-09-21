package adrian.handsonscala

object MergeSort extends App {

  def mergeSort(a: Array[Int]): Array[Int] = {
    if (a.length <= 1)
      a
    else {
      val(left, right) = a.splitAt(a.length / 2)
      combine(mergeSort(left), mergeSort(right))
    }
  }

  def combine(a: Array[Int], b: Array[Int]): Array[Int] = {
    var left = 0
    var right = 0
    val combined = Array.newBuilder[Int]
    while (left < a.length || right < b.length) {
      if (right == b.length || (left < a.length && a(left) <= b(right))) {
        combined += a(left)
        left += 1
      } else {
        combined += b(right)
        right += 1
      }
    }
    combined.result()
  }

  println(mergeSort(Array(1, 9, 2, 8, 5, 5, 0, 4, 6)).mkString(", "))

}
