package main.chapter8

import java.util.concurrent.{ExecutorService, Executors}

import main.chapter6.{SimpleRNG, RNG, State}
import main.chapter5.Stream
import Gen._
import Prop._
import main.chapter7.Par.Par


case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

   def &&(p: Prop): Prop = Prop {
      (maxSize, n, rng) =>
         val firstR = run(maxSize, n, rng)
         if (firstR.isFalsified)
            firstR
         else p.run(maxSize, n, rng)
   }

   def ||(p: Prop): Prop = Prop {
      (maxSize, n, rng) =>
         val firstR = run(maxSize, n, rng)
         if (firstR.isFalsified)
            p.run(maxSize, n, rng)
         else firstR
   }

   def tag(t :String):Prop = Prop((maxSize, n, rng) => {
      run(maxSize, n, rng) match {
         case Passed => Passed
         case Falsified(f, s) => Falsified(t + f, s)
      }
   })


}

object Prop {
   type SuccessCount = Int
   type FailedCase = String
   type MaxSize = Int
   type TestCases = Int
//   type Result = Either[(FailedCase, SuccessCount), SuccessCount]

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

   case object Proved extends Result {
      def isFalsified = false
   }

   def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

   def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
         val casesPerSize = (n + (max - 1)) / max
         val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
         val prop: Prop =
            props
               .map(p => Prop { (max, _, rng) =>
                  p.run(max, casesPerSize, rng)
               })
               .toList
               .reduce(_ && _)
         prop.run(max, n, rng)
   }

   def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (maxSize, n, rng) => randomStream(as)(rng)
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

//   def run(p: Prop,
//           maxSize: Int = 100,
//           testCases: Int = 100,
//           rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
//      p.run(maxSize, testCases, rng) match {
//         case Falsified(msg, n) =>
//            println(s"! Falsified after $n passed tests:\n $msg")
//         case Passed =>
//            println(s"+ OK, passed $testCases tests.")
//      }

   def run(p: Prop,
           maxSize: Int = 100,
           testCases: Int = 100,
           rng: RNG = SimpleRNG(System.currentTimeMillis)): Result = p.run(maxSize, testCases, rng)

   def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
   }

   val S:Gen[ExecutorService] = weighted(
      choose(1,4).map(Executors.newFixedThreadPool) -> .75,
      unit(Executors.newCachedThreadPool) -> .25)

   def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) { case s ** a => f(a)(s).get }

   def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

}

object ** {
   def unapply[A,B](p: (A,B)) = Some(p)
}




