package adrian.blog

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object StreamProgram extends App {

  def program[S[_, _]](streamAlgebra: StreamAlgebra[S]): S[Int, Int] = {
    import streamAlgebra._


    merge(lift((a: Int) => a * 10), lift((a: Int) => a + 1))
  }


  def withAkka: Future[Unit] = {
    implicit val system = ActorSystem.apply(Behaviors.ignore, "actorSystem")
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

    val p = program(new AkkaStream)

    val m = Source(List(1, 2, 3)).via(p).runWith(Sink.foreach(println))
    for {
      _ <- m
      _ <- Future { system.terminate()}
      _ <- system.whenTerminated
    } yield {
      println("shutdown")
    }
  }

  Await.result(withAkka, 5.seconds)




}
