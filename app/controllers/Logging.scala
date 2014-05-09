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
    Ok("/1")
  }

  // For use in testing only
  //  def retrieveData(id: String) = APIAction {
  //    Ok(LoggingHandler.retrieveLogText(id.toLong))
  //  }

  def logData(id: String) = APIAction {
    Ok("Logging temporarily disabled")
  }

}
