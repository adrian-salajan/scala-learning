package main.chapter2


object Chapter2 {

   def main(args: Array[String]): Unit = {

      print(fib(10))

      print(isSorted(Array(1, 3, 4), (a: Int, b: Int) => {a <= b}))

      print(isSorted(Array(1, 2), (a: Int, b: Int) => {a <= b}))

   }


   /**
     * 0, 1, 1, 2, 3, 5
     */
   def fib(n: Int): Int = {

      @annotation.tailrec
      def fibo(n: Int, prev: Int, next: Int): Int = {
         if (n == 0)
            prev
         else
            fibo(n-1, next, prev + next )
      }

      fibo(n, 0, 1)
   }

   def isSorted[A](as: Array[A], orderered: (A, A) => Boolean) :Boolean = {

      def compare(index: Int, as: Array[A], ordered: (A, A) => Boolean, isOrdered: Boolean): Boolean = {
         if (index + 1 >= as.length)
            isOrdered
         else if (isOrdered)
            compare(index + 1, as, ordered, ordered(as(index), as(index + 1)))
         else
            isOrdered
      }

      compare(0, as, orderered, true)
   }


  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  def uncurry[A, B, C](f: A => B => C) : (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }


}
