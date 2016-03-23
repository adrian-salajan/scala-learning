package scala

import scala.views.CreateStory
import scalaz._
import Scalaz._
import scalaz.http._
import response._
import request._
import scalaz.http.request.Request._
import servlet._
import HttpServlet._
import Slinky._
import scala.views._
import scala.model._

class WeKanbanApplication extends StreamStreamServletApplication{

  implicit val charset = UTF8

  override val application: ServletApplication[Stream, Stream] = new ServletApplication[Stream, Stream]() {

    override def application(implicit servlet: HttpServlet,
                             servletRequest: HttpServletRequest,
                             request: Request[Stream]): Response[Stream] = {

      def found(x: Iterator[Byte]) : Response[Stream] = OK << x.toStream

      handle | HttpServlet.resource(found, NotFound.xhtml)
    }

    def handle(implicit servlet: HttpServlet,
                             servletRequest: HttpServletRequest,
                             request: Request[Stream]): Option[Response[Stream]] = {
        request match {
          case MethodParts(GET, "card" :: "create" :: Nil) =>
              Some(
                OK(ContentType, "text/html") << strict <<
                 CreateStory(param("message"))
              )
          case MethodParts(POST, "card" :: "save" :: Nil) =>
             Some(saveStory)

          case _ => None
        }
    }
  }

   private def saveStory(implicit request: Request[Stream],
                         servletRequest: HttpServletRequest) = {
      val title = param_!("title")
      val number = param_!("storyNumber")
      Story(number, title).save match {
         case Right(message) =>
            Response.redirects[Stream, Stream]("/card/create", ("message", message))
         case Left(error) =>
            OK(ContentType, "text/html") << transitional <<
            CreateStory(error.toString)
      }
   }

  def param(name: String)(implicit request: Request[Stream]) = {
    (request ! name).getOrElse(List[Char]()).mkString("")
  }

   def param_!(name: String)(implicit request: Request[Stream]) = {
      (request | name).getOrElse(List[Char]()).mkString("")
   }


}
