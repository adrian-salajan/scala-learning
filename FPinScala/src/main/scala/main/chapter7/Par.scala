package main.chapter7

import java.util.concurrent.{Callable, Future, TimeUnit, ExecutorService}



object Par {

   type Par[A] = ExecutorService => Future[A]


   def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

   def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      (es: ExecutorService) => {
         val af = a(es)
         val bf = b(es)
         UnitFuture(f(af.get, bf.get))
      }

   def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => es.submit(new Callable[A] {
      def call() = a(es).get()
   })

   def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

   def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

   def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

   private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
   }

   def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

   def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

   def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.reverse.foldLeft(unit(List[A]()))((r, par) => map2(r, par)((lr, ep) => ep :: lr))


   def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val filters: List[Par[List[A]]] = as.map(
         asyncF((e) => if (f(e)) List(e) else List())
      )

      val sequence1: Par[List[List[A]]] = sequence(filters)
      map(sequence1)(_.flatten)
   }

   def parCountWords(p: List[String]): Par[Int] = {
      val wordsInChapter: List[Par[Int]] = p.map(asyncF(e => e.split(" ").length))

      map(sequence(wordsInChapter))(l => l.reduceLeft(_ + _))
   }

   def parMapReduce[A, B](l: List[A])(mapF: A => B)(reduceF: (B, B) => B): Par[B] = {
      val mapped: List[Par[B]] = l.map(asyncF(mapF))

      val parList: Par[List[B]] = sequence(mapped)

      map(parList)(li => li.reduce(reduceF))
   }

   def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

   def delay[A](fa: => Par[A]): Par[A] = es => fa(es)


   def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      val r = run(es)(n).get()
      choices(r)(es)
   }
   def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

   //join
   def select[A](a: Par[Par[A]]):Par[A] = es => {
      a(es).get()(es)
   }

   def choiceS[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      select(Par.map(cond)(b => if (true) t else f))
   }

   def choiceNS[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      select(Par.map(n)(i => choices(i)))
   }

   def flatMap[A, B](cond: Par[A])(f: A => Par[B]): Par[B] = es => {
      val a = cond(es).get()
      f(a)(es)
   }

   def flatMapSelect[A, B](cond: Par[A])(f: A => Par[B]): Par[B] =
      select(map(cond)(f))


   //choice -> flatMap WOW!

   def choiceFM[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      Par.flatMap(cond)(x => if (x) t else f)

   def choiceNFM[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      Par.flatMap(n)(i => choices(i))
   }

   def selectFM[A](a: Par[Par[A]]):Par[A] =
      flatMap(a)(id => id)
}


