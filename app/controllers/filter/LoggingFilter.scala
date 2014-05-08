package controllers.filter

import scala.concurrent.Future

import
  play.api.{ libs, Logger, mvc },
    libs.concurrent.Execution.Implicits.defaultContext,
    mvc.{ Filter, RequestHeader, SimpleResult }

object LoggingFilter extends Filter {

  def apply(nextFilter: (RequestHeader) => Future[SimpleResult])(requestHeader: RequestHeader): Future[SimpleResult] = {
    val startTime = System.currentTimeMillis
    nextFilter(requestHeader) map {
      result =>
        Logger.info(s"${requestHeader.method} ${requestHeader.uri} took ${System.currentTimeMillis - startTime}ms and returned ${result.header.status}")
        result
    }
  }

}
