package scala.model

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations._
import KanbanSchema._
/**
  * Created by adrian on 20/3/2016.
  */
class Story(val number: String, val title: String, val phase: String) {

   private[this] def validate = {
      if (number.isEmpty || title.isEmpty) {
         throw new ValidationException("Number and title are required.")
      }

      if(!stories.where(a =>  a.number === number).isEmpty) {
         throw new ValidationException("Story number must be unique.")
      }
   }

   def save(): Either[Throwable, String] = {
      tx {
         try {
            validate
            stories.insert(this)
            Right("Story created")
         } catch {
            case e: Throwable => Left(e)
         }
      }
   }

}

object Story {
   def apply(number: String, title:String) = {
      new Story(number, title, "ready")
   }
}

class ValidationException(message: String) extends RuntimeException(message)