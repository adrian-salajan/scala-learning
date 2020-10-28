package adrian.rockthejvm.streams

import akka.NotUsed
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed._
import akka.stream.{ActorAttributes, Attributes, Supervision}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.typed.scaladsl.ActorFlow
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


object AsyncMapAsync extends App {

  val as = ActorSystem(main, "actor-system-for-async")

  println("start")

  Console.in.readLine()
  print("goodbye")
  as.terminate()


  def main: Behavior[NotUsed] = Behaviors.setup {
    ac => start(ac)
  }


  private def start(ac: ActorContext[NotUsed]): Behavior[NotUsed] = {
    val someActor1: ActorRef[Command] =
      ac.spawn(someActorBehavior, "some-actor1", Props.empty.withDispatcherFromConfig("actor-dispatcher"))
    val someActor2: ActorRef[Command] =
      ac.spawn(someActorBehavior, "some-actor2", Props.empty.withDispatcherFromConfig("actor-dispatcher"))

    implicit val askTimeout: Timeout = Timeout(2000.millis)
    implicit val scheduler: ActorSystem[Nothing] = ac.system
//    implicit val scheduler: Scheduler = ac.system.scheduler
//    implicit val materializer: Materializer  = ac.system.
    implicit val ecForFutures: ExecutionContext =
      ac.system.dispatchers.lookup(DispatcherSelector.fromConfig("future-dispatcher"))

    val start = System.nanoTime()
    val future = Source(1 to 600)
      .via(
//               Flow.apply[Int] .mapAsyncUnordered(8) { n => someActor1.ask[Reply](replyTo => Command(n, replyTo)).map(_.n) }
        ActorFlow.ask(parallelism = 1)(someActor1)((n: Int, replyTo: ActorRef[Reply]) => Command(n, replyTo))
        .map(_.n)
        .named("stage-1")
      )



//                .async

      .via(
//        Flow.apply[Int].mapAsyncUnordered(8) { n => someActor2.ask[Reply](replyTo => Command(n, replyTo)).map(_.n) }
        ActorFlow.ask(parallelism = 1)(someActor2)((n: Int, replyTo: ActorRef[Reply]) => Command(n, replyTo))
        .map(_.n)
        .named("stage-2")
      )

//                .async


      .toMat(Sink.foreach(println))(Keep.right)
      .addAttributes(ActorAttributes.supervisionStrategy(_ => Supervision.Resume) )
      .named("full-stream-1")
      .run()

    future.onComplete {
      case scala.util.Success(_) => println("time 2 = " + (System.nanoTime() - start) / 1000000000)
    }

    Behaviors.receiveSignal {
      case (_, Terminated(_)) =>
        println("Terminating")
        Behaviors.stopped
    }
  }

  def someActorBehavior: Behaviors.Receive[Command] = Behaviors.receiveMessage[Command] { c =>
    Thread.sleep(50)
    c.replyTo ! Reply(c.n * 10)
    Behaviors.same
  }

  case class Command(n: Int, replyTo: ActorRef[Reply])
  case class Reply(n: Int)

}






