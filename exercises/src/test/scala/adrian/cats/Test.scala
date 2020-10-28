package adrian.cats

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {

  test("empty") {
      import alleycats.Empty
  import cats.implicits._

    assert(Option(3).flatTap(_ => None) == None)

      // works fine
      Empty[List[Int]]
      Empty[Set[Int]]
      Empty[Stream[Int]]

//    case class A(a: String)
      // doesn't work by default
//      println(">>>> "+ Empty[Option[A]].empty)
  }

}
