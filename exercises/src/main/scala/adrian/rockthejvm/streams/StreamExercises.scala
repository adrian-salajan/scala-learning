package adrian.rockthejvm.streams

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.{FlowShape, SourceShape, UniformFanInShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, MergePreferred, Sink, Source, Zip, ZipWith}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object StreamExercises extends App {

  implicit val system = ActorSystem(Behaviors.ignore, "actorSystem")
  implicit val ec = scala.concurrent.ExecutionContext.global

  //  val lastElement = Source(List(1, 2, 3)).runWith(Sink.last)
  //  val lastElement = Source(List(1, 2, 3)).toMat(Sink.last)(Keep.right).run()


  //  lastElement.onComplete {
  //    case Success(value) => println(s"last element is $value")
  //    case Failure(value) => println(s"last element unavailable $value")
  //  }

  val sentences = List("Ana are mere", "si pere", "ala bala", "portocala")

  //  val wordCount = Source(sentences)
  //    .map(_.split(" ").length)
  //    .toMat(Sink.fold(0)((z, n) => z + n))(Keep.right)
  //    .run()

  //  val wordCount = Source(sentences)
  //    .via(Flow[String].flatMapConcat(s => Source(s.split(" "))))
  //    .toMat(Sink.fold(0)((z, _) => z + 1))(Keep.right)
  //
  //  val wordCount = Source(sentences)
  //    .via(Flow[String].flatMapConcat(s => Source(s.split(" "))))
  //    .runWith(Sink.foldAsync[Int, String](0)((z, _) => Future(z + 1)))

  //  val wordCount = Source(sentences)
  //    .via(Flow[String].flatMapConcat(s => Source(s.split(" "))))
  //      .runFold(0)((z, _) => z + 1)
  //
  //
  //  wordCount.onComplete {
  //    case Success(value) => println(s"wc is $value")
  //    case Failure(value) => println(s"wc unavailable $value")
  //  }

  //  val futureCount = Source(sentences).viaMat(Components.flowEnhanced(Flow.apply))(Keep.right).toMat(Sink.ignore)(Keep.left).run()
  //
  //  futureCount.onComplete(_.fold(e => println(e), c => println(s"count was $c")))

  Components.fibonaci.take(10).runWith(Sink.foreach(println(_)))


}

object Components {


  def fibonaci: Source[Int, NotUsed] = Source.fromGraph {
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val BBroadcaster = builder.add(Broadcast[Int](2))
      val ABroadcaster = builder.add(Broadcast[Int](2))
      val ABMergePreferred = builder.add(MergePreferred[Int](1))
      val BSumMergePreferred = builder.add(MergePreferred[Int](1))
      val sum = builder.add(ZipWith[Int, Int, Int]((a, b) => a + b))


      Source.single(0) ~> ABMergePreferred.in(0)
      BBroadcaster ~> ABMergePreferred.preferred
      ABMergePreferred ~> ABroadcaster.in

      Source.single(1) ~> BSumMergePreferred.in(0)
      BSumMergePreferred ~> BBroadcaster

      ABroadcaster ~> sum.in0
      BBroadcaster.out(1) ~> sum.in1

      sum.out ~> BSumMergePreferred.preferred


      SourceShape(ABroadcaster.out(1))
    }
  }


  def flowEnhanced[A, B](flow: Flow[A, B, _]): Flow[A, B, Future[Int]] = {
    val counter = Sink.fold[Int, B](0)((a, _) => a + 1)
    Flow.fromGraph {
      GraphDSL.create(counter) { implicit builder =>
        counterShape =>
          import GraphDSL.Implicits._

          val flowShape = builder.add(flow)
          val broadcast = builder.add(Broadcast[B](2))

          flowShape ~> broadcast
          broadcast ~> counterShape

          FlowShape(flowShape.in, broadcast.out(1))
      }
    }
  }
}


