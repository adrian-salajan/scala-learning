package main.chapter3

/**
  * Created by adrian on 5/8/2016.
  */
object Chapter3 {

   def main(args: Array[String]) = {
      val l: List[Int] = List(1,2,3,4,5)
      val x = l match {
         case x :: 2 :: 4 :: _ => x
         case Nil => 42
         case x :: y :: 3 :: 4 :: _ => x + y
         case h :: t => h
         case _ => 101
      }

   print(x) // 1 + 2 = 3
   }


}
