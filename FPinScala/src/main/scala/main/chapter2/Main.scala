package main.chapter2

object Main {

   def main(args: Array[String]): Unit = {

      print(fib(10))

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

}
