package main.chapter9

trait JSON

object JSON {

   case object JNull extends JSON

   case class JNumber(get: Double) extends JSON

   case class JString(get: String) extends JSON

   case class JBool(get: Boolean) extends JSON

   case class JArray(get: IndexedSeq[JSON]) extends JSON

   case class JObject(get: Map[String, JSON]) extends JSON




//   def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
//      import P._
//      val spaces = char(' ').many.slice
//
//      def jvalue: Parser[JSON] = jliteral | jarray | jobject
//
//      def jliteral: Parser[JSON] =
//         string("null").map(s => JNull) |
//         regex("\\d+/.\\d+".r).map(ds => JNumber(ds.toDouble)) |
//         regex("\"[A-Za-z]*\"".r).map(s => JString(s))
//         string("true").map(_ => JBool(true)) |
//         string("false").map(_ => JBool(true))
//
//      def jarray: Parser[JArray] =
//         surroundedBy(
//            enumeration(jvalue, char(',')).map(li => JArray(li.toIndexedSeq)),
//            char('['),
//            char(']')
//         )
//
//      def jobject: Parser[JSON] = surroundedBy(
//         enumeration(
//            pair(regex("\"[A-Za-z]*\"".r), char(':'), jliteral),
//            char(',')),
//         char('{'),
//         char('}')
//      ).map(kv => JObject(kv.toMap))
//
//
//
//      jobject
//   }
}
