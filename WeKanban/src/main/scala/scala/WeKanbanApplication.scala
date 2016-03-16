package scala

import scalaz.http.Slinky._
import scalaz.http._
import scalaz.http.request._
import scalaz.http.response._
import scalaz.http.servlet._

class WeKanbanApplication extends StreamStreamServletApplication{

  override val application: ServletApplication[Stream, Stream] = new ServletApplication[Stream, Stream]() {

    override def application(implicit servlet: HttpServlet,
                             servletRequest: HttpServletRequest,
                             request: Request[Stream]): Response[Stream] = {

      HttpServlet.resource(x => OK.<<(x.toStream), NotFound.xhtml)
    }
  }
}
