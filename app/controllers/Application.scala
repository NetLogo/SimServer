package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.BodyParsers.parse
import models.log.LoggingHandler
import models.hubnet.HubNetServerManager
import models.JNLPProps
import models.util.{DecryptionUtil, RequestUtil}

object Application extends Controller {

  val LoggingDataKey = "logging_data"
  //@ Do something on startup to clear out autogen files

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def ws = Action {
    Ok(views.html.ws())
  }
  
  def startLogging = Action {
    Ok("/" + LoggingHandler.createNewLog())
  }

  //@ For testing only.  This has potential for trouble.
  // In the code that ends up being deployed, people should not be able to see other people's logs!
  def retrieveData(id: String) = Action {
    Ok(LoggingHandler.retrieveLogText(id.toLong))
  }

  def logData(id: String) = Action {
    request =>
      val data = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                    orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                    orElse(Option(request.queryString)).
                    flatMap(_.get(LoggingDataKey)).flatMap(_.headOption).
                    flatMap(str => if (LoggingHandler.isHandlable(str)) Some(str) else RequestUtil.extractPropertyFromUri(request.uri, LoggingDataKey)).
                    getOrElse ("ERROR_IN_PARSING ")
      val response = LoggingHandler.log(id.toLong, data)
      Ok(response)
  }
  
  def hubnet(hash: String) = Action {
    request =>
      DecryptionUtil.decodeForHubNet(hash) flatMap {
        case (modelNameOpt, username, teacherName, isTeacher) =>
          val portMaybe = {
            import HubNetServerManager._
            if (isTeacher) startUpServer(modelNameOpt, teacherName) else getPortByTeacherName(teacherName)
          }
          val serverPath = request.uri //@ Does this work?  (Will probably have to trim it, at the very least)
          val propsMaybe = portMaybe map (port => JNLPProps(modelNameOpt, Option(username), Option(serverPath), Option(port)))
          propsMaybe map ( /* Generate file (name "<hash>.jnlp"); create actor that will destroy it within a minute; return URL */ )
      } fold (ExpectationFailed(_), Redirect(_))
  }
  
}
