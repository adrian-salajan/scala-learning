package main.chapter9

import main.chapter8._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

   def run[A](p: Parser[A])(input: String): Either[ParseError,A]

   // PRIMITIVES
   def string(s: String): Parser[String]
   implicit def regex(r: Regex): Parser[String]
   def slice[A](p: Parser[A]): Parser[String]
   def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
   def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

   def matchCondition[A](p: Parser[A]): String
   // NON - PRIMITIVES
   def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

   def min[A](n: Int, a: A): Parser[Int]

   def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      // map2(p, p)((a, _) => f(a))
      flatMap(p)(a => succeed(f(a)))

   def succeed[A](a: A): Parser[A] = string("") map (_ => a)

   //def many[A](p: Parser[A]): Parser[List[A]]

   //many in terms of or, map2, and succeed.
   def many[A](p: Parser[A]): Parser[List[A]] = map2(
      p,
      or(many(p),
         succeed(List[A]())))((a, l) => a :: l)

//   def many[A](p: Parser[A]): Parser[List[A]] =
//      map2(p, many(p))(_ :: _) or succeed(List())

   def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))((a, b) => a :: b)

   def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = flatMap(p)(pa => map(p2)(pb => (pa, pb)))

  // def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = map(product(p, p2))(t => f(t._1, t._2))

   def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      flatMap(p)(pa => map(p2)(pb => f(pa, pb)))

   def enumeration[A, B](p: Parser[A], separator: Parser[B]): Parser[List[A]] =
      map2(
            p,
            many(map2(separator, p)((s, pe) => pe))
      )(_ :: _)

   def beginsWith[A, B](p: Parser[A], begin: Parser[B]) =
      map2(begin, p)((b, e) => e)

   def endsWith[A, B](p: Parser[A], ends: Parser[B]) =
      map2(p, ends)((e, en) => e)

   def surroundedBy[A,B](p: Parser[A], begin: Parser[B], end: Parser[B]) =
      endsWith(beginsWith(p, begin), end)

   def pair[A, B, C](a: Parser[A], separator: Parser[B], b: Parser[C]): Parser[(A, C)] =
      flatMap(a)(left =>
         flatMap(separator)(s =>
            map(b)(right => (left, right))
         )
      )



//   Using map2 and succeed, implement the listOfN combinator from earlier.
   def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) succeed(List())
      else map2(p, listOfN(n-1, p))(_ :: _)


   def thatMany[A](that: Parser[A], many: Parser[Int]) = flatMap(many)(m => listOfN(m, that))

   def thatManyChars(c: Char) = thatMany(char(c), regex("\\d".r).map(d => d.toInt))

   def escapedString(s: String) = string(s)

   case class Location(input: String, offset: Int = 0) {
      lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
      lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
         case -1 => offset + 1
         case lineStart => offset - lineStart
      }
   }

   def errorLocation(e: ParseError): Location
   def errorMessage(e: ParseError): String

   def label[A](msg: String)(p: Parser[A]): Parser[A]
   def scope[A](msg: String)(p: Parser[A]): Parser[A]

   case class ParseError(stack: List[(Location,String)])



   implicit def strings(s: String): Parser[String] = string(s)
   implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
   implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

   case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]) = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
//      def onFail(message :String) = self.onFail(p, message)
//      def withFailure() = self.withFailure(p)
      def many = self.many(p)
      def slice = self.slice(p)
      val numA: Parser[Int] = char('a').many.map(_.size)
      val numA2 = char('a').many.slice.map(_.length)
   }

   object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
      def unitLaw[A](in: Gen[String]): Prop = Prop.forAll(in)(s => run(succeed("x"))(s) == Right("x"))


   }


}

