package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.BodyParsers.parse
import models.log.LoggingHandler
import models.hubnet.HubNetServerManager
import models.JNLPProps
import models.util.{TempGenManager, DecryptionUtil, RequestUtil}

object Application extends Controller {

  val LoggingDataKey = "logging_data"

  TempGenManager.removeAll()  // Clear all temp gen files on startup

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
  
  def handleHubNet(input: String) = Action {
    request =>
      DecryptionUtil.decodeForHubNet(input) flatMap {
        case (modelNameOpt, username, teacherName, isTeacher) =>

          val portMaybe = {
            import HubNetServerManager._
            if (isTeacher) startUpServer(modelNameOpt, teacherName) else getPortByTeacherName(teacherName)
          }

          // Regex to find the first '/' that is alone and split string there
          // ( http://www.derpy.com:9000/stupid/crap => (http://www.derpy.com:9000, stupid/crap) )
          val URISplitter = """(.*?)(?<!/)/(?!/)(.*)""".r
          val URISplitter(serverPath, _) = request.uri  //@ Does this work?  (Is it even necessary?)

          val propsMaybe = portMaybe map (port => JNLPProps(modelNameOpt, Option(username), Option(serverPath), Option(port)))
          val fileContentsAndNameMaybe = propsMaybe flatMap ( x => scalaz.Success("", "")/* Generate file (name "<hash>.jnlp") */ ) //@ Do it
          fileContentsAndNameMaybe map { case (contents, name) => TempGenManager.registerFile(contents, name).toString }

      } fold (ExpectationFailed(_), Redirect(_))
  }

}
