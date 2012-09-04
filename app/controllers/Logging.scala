package controllers

import play.api._
import play.api.mvc._
import models.log.LoggingHandler

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object Logging extends Controller {

  val LoggingDataKey = "logging_data"

  def ws = Action {
    Ok(views.html.ws())
  }

  def startLogging = APIAction {
    Ok("/" + LoggingHandler.createNewLog())
  }

  //@ For testing only.  This has potential for trouble.
  // In the code that ends up being deployed, people should not be able to see other people's logs!
  def retrieveData(id: String) = APIAction {
    Ok(LoggingHandler.retrieveLogText(id.toLong))
  }

  def logData(id: String) = APIAction {
    request =>
      val data = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                         orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                         orElse(Option(request.queryString)).
                         flatMap(_.get(LoggingDataKey)).flatMap(_.headOption).
                         map(LoggingHandler.log(id.toLong, _))
      val response = data getOrElse ("ERROR_IN_PARSING")
      Ok(response)
  }

}
