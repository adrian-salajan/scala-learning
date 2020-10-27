package adrian.blog

import akka.stream.scaladsl.{Flow, GraphDSL, Sink}
import akka.NotUsed
import akka.stream.Graph

class AkkaStream extends StreamAlgebra[Flow[*, *, NotUsed]] {


  override def merge[A, B, C](a: Flow[A, B, NotUsed], b: Flow[B, C, NotUsed]): Flow[A, C, NotUsed] =
    a.via(b)

  override def lift[A, B](processor: A => B): Flow[A, B, NotUsed] =
    Flow.fromFunction(processor)

  override def divert[A, B, C](a: Flow[A, B, NotUsed])(f: B => Boolean)(onTrue: Flow[B, C, NotUsed]): Flow[A, B, NotUsed] = {
    a.divertTo(onTrue.to(Sink.ignore), f)
  }
}
