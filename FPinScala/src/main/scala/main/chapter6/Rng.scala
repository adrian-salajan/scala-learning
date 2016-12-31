package main.chapter6

trait RNG {
   def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
   def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
   }
}

object RNG {
   def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (n, r) = rng.nextInt
      if (n == Integer.MIN_VALUE) return (Integer.MAX_VALUE, r)
      (Math.abs(n), r)
   }

   // [0. 1)
   def double(rng: RNG): (Double, RNG) = {
      val (n, r) = nonNegativeInt(rng)
      ((n % 10).toDouble / n, r)
   }
   def intDouble(rng: RNG): ((Int,Double), RNG) =  {
      val (n, r) = rng.nextInt
      val (m, rr) = RNG.double(r)
      ((n, m), rr)
   }
   def doubleInt(rng: RNG): ((Double,Int), RNG)  = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
   }
   def double3(rng: RNG): ((Double,Double,Double), RNG)  = {
      val (a, x) = double(rng)
      val (b, y) = double(x)
      val (c, z) = double(y)
      ((a, b, c), z)
   }
   def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
      case x if x == 0 => (List(), rng)
      case _ =>
         val (n, nextR) = rng.nextInt
         val (listN, lastR) = ints(count - 1)(nextR)
         (n :: listN, lastR)
   }

   type Rand[+A] = RNG => (A, RNG)

   val int: Rand[Int] = _.nextInt

   def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
         val (a, rng2) = s(rng)
         (f(a), rng2)
      }
   def doubleMap: Rand[Double] = map(nonNegativeInt)(i => (i % 10).toDouble / i)

   def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
         val (a, rng1) = ra(rng)
         val (b, rng2) = rb(rng1)
         (f(a, b), rng2)
      }

   def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_, _))

   val randIntDouble: Rand[(Int, Double)] = both(int, double)
   val randDoubleInt: Rand[(Double, Int)] = both(double, int)

   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs match {
         case (h :: t) if t != Nil => map2(h, sequence(t))((a, b) => a :: b)
         case (h :: Nil) => RNG.map(h)(List(_))
   }

   def unit[A](a: A): Rand[A] = rng => (a, rng)

   def sequenceFold[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(RNG.unit(List[A]()))((r, li) => map2(r, li)(_ :: _))

   def intsSequence(count: Int): Rand[List[Int]] = {
      val rands = List.fill(count)(int)
      RNG.sequence(rands)
   }

   def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
         val (a, r1) = f(rng)
         g(a)(r1)
      }

   def nonNegativeLessThan(n: Int): Rand[Int] = RNG.flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
   })

   def mapF[A,B](s: Rand[A])(f: A => B): Rand[B] = RNG.flatMap(s)(a => RNG.unit(f(a)))

   def map2F[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      RNG.flatMap(ra)(a => RNG.map(rb)(b => f(a,b)))
}
