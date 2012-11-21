package controllers

import play.api._
import play.api.mvc._
import models.log.LoggingHandler
import models.util.PlayUtil

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object Logging extends Controller {

  val LoggingDataKey = "logging_data"

  def startLogging = APIAction {
    Ok("/" + LoggingHandler.createNewLog())
  }

  // For use in testing only
  //  def retrieveData(id: String) = APIAction {
  //    Ok(LoggingHandler.retrieveLogText(id.toLong))
  //  }

  def logData(id: String) = APIAction {
    request =>
      val paramMapOpt = PlayUtil.extractParamMapOpt(request)
      val status = for {
        paramMap   <- paramMapOpt
        logDataSeq <- paramMap.get(LoggingDataKey)
        logData    <- logDataSeq.headOption
      } yield (LoggingHandler.log(id.toLong, logData))
      status map (Ok(_)) getOrElse BadRequest("ERROR_IN_PARSING")
  }

}
