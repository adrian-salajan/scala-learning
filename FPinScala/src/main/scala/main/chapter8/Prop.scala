package main.chapter8

import main.chapter6.RNG
import main.chapter5.Stream
import main.chapter6.State
import Gen._
import Prop._

case class Prop(run: (TestCases, RNG) => Result) {

   def &&(p: Prop): Prop = Prop {
      (n, rng) =>
         val firstR = run(n, rng)
         if (firstR.isFalsified)
            firstR
         else p.run(n, rng)
   }

   def ||(p: Prop): Prop = Prop {
      (n, rng) =>
         val firstR = run(n, rng)
         if (firstR.isFalsified)
            p.run(n, rng)
         else firstR
   }

   def tag(t :String):Prop = Prop((n, rng) => {
      run(n, rng) match {
         case Passed => Passed
         case Falsified(f, s) => Falsified(t + f, s)
      }
   })


}

object Prop {
   type SuccessCount = Int
   type FailedCase = String

   type TestCases = Int
   //type Result = Either[(FailedCase, SuccessCount), SuccessCount]

   sealed trait Result {
      def isFalsified: Boolean
   }
   case object Passed extends Result {
      def isFalsified = false
   }
   case class Falsified(failure: FailedCase,
                        successes: SuccessCount) extends Result {
      def isFalsified = true
   }

   def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (n,rng) => randomStream(as)(rng)
         .zip(Stream.from(0))((_,_))
         .take(n)
         .map {
            case (a, i) =>
               try {
                  if (f(a)) Passed else Falsified(a.toString, i)
               } catch {
                  case e: Exception => Falsified(buildMsg(a, e), i)
               }
         }
         .find(_.isFalsified).getOrElse(Passed)
   )

   def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

   def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
         s"generated an exception: ${e.getMessage}\n" +
         s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}




