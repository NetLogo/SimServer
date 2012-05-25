package controllers

import play.api._
import play.api.mvc._
import models.log.LoggingHandler
import models.hubnet.HubNetServerManager
import models.util.{TempGenManager, DecryptionUtil, RequestUtil}
import java.net.URI
import models.jnlp.{MainJar, JNLP}

object Application extends Controller {

  val LoggingDataKey = "logging_data"
  val HubNetKey      = "hubnet_data"
  val ModelsSubDir   = "assets/misc/models"
  val DepsSubDir     = "assets/misc/deps"

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
  
  def handleHubNet = Action {
    request =>
      val inputMaybe = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                               orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                               orElse(Option(request.queryString)).
                               flatMap(_.get(HubNetKey)).flatMap(_.headOption) map (scalaz.Success(_)) getOrElse (scalaz.Failure("Invalid POST data"))
      val decryptedMaybe = inputMaybe flatMap (DecryptionUtil.decodeForHubNet(_))
      decryptedMaybe flatMap {
        case (modelNameOpt, username, teacherName, isTeacher) =>

          val portMaybe = {
            import HubNetServerManager._
            if (isTeacher) startUpServer(modelNameOpt, teacherName) else getPortByTeacherName(teacherName)
          }

          // Regex to find the first '/' that is alone and split string there
          // ( http://www.derpy.com:9000/stupid/crap => (http://www.derpy.com:9000, stupid/crap) )
          val URISplitter = """(.*?)(?<!/)/(?!/)(.*)""".r
          val URISplitter(serverPath, _) = request.uri
          val modelJNLPName = modelNameOpt getOrElse "NetLogo"

          val propsMaybe = portMaybe map {
            port => JNLP(
              new URI(serverPath),
              TempGenManager.formatFilePath(inputMaybe.toOption.get + ".jnlp"), // Impossible for `inputMaybe` to be a `Failure` at this point
              new MainJar("NetLogo.jar"),  //@ Subject to change
              "%s HubNet Client".format(modelJNLPName),
              "org.nlogo.hubnet.client.App",
              "NetLogo HubNet Client",
              "A HubNet client for %s".format(modelJNLPName),
              "HubNet (%s)".format(modelJNLPName),
              false,
              properties = Seq("hubnet.username" -> username, "hubnet.serverpath" -> serverPath, "hubnet.port" -> port.toString),
              modelName = modelNameOpt map (name => "%s/%s/%s".format(serverPath, ModelsSubDir, name + ".nlogo"))
            )
          }
          propsMaybe map (jnlp => TempGenManager.registerFile(jnlp.toXMLStr, jnlp.jnlpLoc).toString)

      } fold (ExpectationFailed(_), Redirect(_))
  }

}
