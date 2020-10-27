package adrian.blog



trait StreamAlgebra[Stream[_, _]] {

  type Sink[A] = Stream[A, Unit]
  def merge[A, B, C](a: Stream[A, B], b: Stream[B, C]): Stream[A, C]
  def divert[A, B, C](a: Stream[A, B])(f: B => Boolean)(onTrue: Stream[B, C]): Stream[A, B]

  def lift[A, B](processor: A => B): Stream[A, B]


}

object StreamAlgebra {

}
