package controllers

import
  play.api.mvc.Controller

import
  controllers.action.APIAction

import
  models.{ log, util },
    log.LoggingHandler,
    util.PlayUtil

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
      val paramBundle      = PlayUtil.extractBundle(request)
      val logDataOpt       = paramBundle.stringParams.get(LoggingDataKey)
      val responseMaybeOpt = logDataOpt map (LoggingHandler.log(id.toLong, _))
      val response         =
        responseMaybeOpt map {
          _ fold (failMsg => RequestTimeout(failMsg.list.mkString("\n")), successMsg => Ok(successMsg))
        } getOrElse (
          BadRequest("ERROR_IN_PARSING")
        )
      response
  }

}
