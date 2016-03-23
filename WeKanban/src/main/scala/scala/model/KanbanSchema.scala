package scala.model

import java.sql.DriverManager

import org.squeryl.PrimitiveTypeMode._
import org.squeryl._
import org.squeryl.adapters._


object KanbanSchema extends Schema {

   val stories = table[Story]("STORIES")

   def init = {
      import org.squeryl.SessionFactory

      Class.forName("org.h2.Driver")

      if (SessionFactory.concreteFactory.isEmpty) {
         SessionFactory.concreteFactory = Some(
            () => Session.create(
               DriverManager.getConnection("jdbc:h2:~/test",
               "sa", ""), new H2Adapter)
         )

      }
   }

   def tx[A](block: => A) : A = {
      init
      inTransaction(block)
   }


   def main(args: Array[String]) {
      println("init weKanban schema..")
      init
      inTransaction{ drop; create}
   }
}
