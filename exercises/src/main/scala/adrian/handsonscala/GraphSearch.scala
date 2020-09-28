package adrian.handsonscala

import scala.collection.mutable

object GraphSearch extends App {


  def searchBF[T](graph: Map[T, Seq[T]], start: T): Set[T] = {
    val seen = mutable.Set(start)
    val queue = mutable.ArrayDeque(start)

    while (queue.nonEmpty) {
      for (n <- graph(queue.removeHead())) {
        if (!seen.contains(n)) {
          seen.add(n)
          queue.append(n)
        }
      }
    }
    seen.toSet
  }


  val bf1 = searchBF(

    graph = Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("c", "d"),
      "c" -> Seq("d"),
      "d" -> Seq()
    ), start = "c" )
  assert(bf1 == Set("c", "d"))
  //----------

  val bf2 = searchBF(

    graph = Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("c", "d"),
      "c" -> Seq("d"),
      "d" -> Seq()
    ), start = "a")
  assert(bf2 == Set("a", "b", "c", "d"))

  val bf3 = searchBF(

    graph = Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("x", "y"),
      "c" -> Seq(),
      "x" -> Seq("xx"),
      "y" -> Seq("yy"),
      "xx" -> Seq(),
      "yy" -> Seq(),
    ), start = "a")
  assert(bf3 == Set("a", "b", "c", "x", "y", "xx", "yy"))

  def searchDF[T](graph: Map[T, Seq[T]], start: T): Set[T] = {
    val seen = mutable.Set(start)
    val queue = mutable.ArrayDeque(start)

    while (queue.nonEmpty)
      for (n <- graph(queue.removeHead())) {
        if (!seen.contains(n)) {
          seen.add(n)
          queue.prepend(n)
        }
    }
    seen.toSet
  }

  val df2 = searchDF(

    graph = Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("c", "d"),
      "c" -> Seq("d"),
      "d" -> Seq()
    ), start = "a")
  assert(df2 == Set("a", "b", "c", "d"))

  val df3 = searchDF(

    graph = Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("x", "y"),
      "c" -> Seq(),
      "x" -> Seq("xx"),
      "y" -> Seq("yy"),
      "xx" -> Seq(),
      "yy" -> Seq(),
    ), start = "a")
  assert(df3 == Set("a", "b", "x", "xx", "y", "yy", "c"))
}