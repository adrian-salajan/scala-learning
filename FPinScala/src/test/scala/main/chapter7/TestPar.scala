package main.chapter7

import java.util.concurrent.{TimeUnit, Executors, ExecutorService, ThreadPoolExecutor}

import main.Test
import main.chapter7.Par.Par
import org.scalatest.concurrent.{TimeLimits, Timeouts}
import org.scalatest.time.{Milliseconds, Span}

/**
  * Created by adrian on 14/1/2017.
  */
class TestPar extends Test with TimeLimits{

   "async" should "work" in {
      val f = (n: Int) => {
         try {
            Thread.sleep(2000);
         }
         catch  {
            case e:InterruptedException =>
         }
         n
      }

      val time = System.currentTimeMillis();
      f(3)
      val execTime = System.currentTimeMillis() - time

      assert(execTime < 2200)

      val asyncf = Par.asyncF(f)
      val executor = Executors.newSingleThreadExecutor()
      val time2 = System.currentTimeMillis();
      Par.run(executor)(asyncf(3))
      val execTime2 = System.currentTimeMillis() - time2

      assert(execTime2 < 100)
   }

   "sequence" should "work" in {
      val l = List[Par[Int]](
         Par.lazyUnit({ Thread.sleep(2000); 1}),
         Par.lazyUnit({ Thread.sleep(2000); 7}),
         Par.lazyUnit({ Thread.sleep(2000); 3})
      )

      val time = System.currentTimeMillis()
      val parlL: Par[List[Int]] = Par.sequence(l)
      val duration = System.currentTimeMillis() - time

      assert(duration < 100)

      val r = Par.run(Executors.newSingleThreadExecutor())(parlL).get()

      r shouldBe List(1, 7, 3)

   }

   "parFilter" should "work" in {
      val l = List(1, 2, 3, 4, 5, 6, 7, 8)
      val parFilter = Par.parFilter(l)(_ % 2 == 0)

      Par.run(Executors.newSingleThreadExecutor())(parFilter).get() shouldBe List(2, 4, 6, 8)
   }

   "parCountWords" should "work" in {
      val ps = List("go to market", "get some apples", "dont forget to pay") //10 words

      Par.run(Executors.newFixedThreadPool(2))(Par.parCountWords(ps)).get shouldBe 10
   }

   "parMapReduce" should "par Count Words" in {
      val ps = List("go to market", "get some apples", "dont forget to pay") //10 words

      val countWords = Par.parMapReduce(ps)(c => c.split(" ").length)(_ + _)

      Par.run(Executors.newFixedThreadPool(2))(countWords).get shouldBe 10
   }

   /*
      Hard: Show that any fixed-size thread pool can be made to deadlock given this implementation of fork.
    */
   "fork" should "deadlock on any bounded thread pool" in {
      val threads = 3

      val S = Executors.newFixedThreadPool(threads)
      val exp = Par.lazyUnit(42 + 1)
      val forks = List.fill(threads)(0).foldLeft(exp)((a, b)=> Par.fork(a))

      // Par.equal(S)(exp, forks)
   }


   "choiceN" should "work" in {
      val choice = Par.choiceN(Par.unit(2))(List(Par.unit(1), Par.unit(2), Par.unit(3)))
      Par.run(Executors.newSingleThreadExecutor())(choice).get() shouldBe 3
   }

   "choice" should "work" in {
      val choice = Par.choice(Par.unit(false))(Par.unit(1), Par.unit(2))
      Par.run(Executors.newSingleThreadExecutor())(choice).get() shouldBe 2
   }

   "choiceNFM" should "work" in {
      val choice = Par.choiceNFM(Par.unit(2))(List(Par.unit(1), Par.unit(2), Par.unit(3)))
      Par.run(Executors.newSingleThreadExecutor())(choice).get() shouldBe 3
   }

   "choiceFM" should "work" in {
      val choice = Par.choiceFM(Par.unit(false))(Par.unit(1), Par.unit(2))
      Par.run(Executors.newSingleThreadExecutor())(choice).get() shouldBe 2
   }



}
