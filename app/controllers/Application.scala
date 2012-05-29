package controllers

import play.api._
import play.api.mvc._
import models.log.LoggingHandler
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

  //@ For testing only
  def hubTest = Action {
    Ok(views.html.hubtest())
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

          //@ This needs to get added back in eventually!
          //val portMaybe = {
            //import HubNetServerManager._
            //if (isTeacher) startUpServer(modelNameOpt, teacherName) else getPortByTeacherName(teacherName)
          //}

          val portMaybe: scalaz.Validation[String, Int] = scalaz.Success(9173) //@

          val host = "http://" + request.host
          val hostIP = "129.105.107.206" //@ Eventually, do this properly
          val modelJNLPName = modelNameOpt getOrElse "NetLogo"
          val filename = inputMaybe.toOption.get  // Impossible for `inputMaybe` to be a `Failure` at this point
          val fileExt = ".jnlp"

          val propsMaybe = portMaybe map {
            port => JNLP(
              new URI(host),
              TempGenManager.formatFilePath(filename + fileExt),
              new MainJar("NetLogo.jar"),
              "%s HubNet Client".format(modelJNLPName),
              "org.nlogo.hubnet.client.App",
              "NetLogo HubNet Client",
              "A HubNet client for %s".format(modelJNLPName),
              "HubNet (%s)".format(modelJNLPName),
              false,
              arguments = Seq("--id", username, "--ip", hostIP, "--port", port.toString)
            )
          }
          propsMaybe map (jnlp => TempGenManager.registerFile(jnlp.toXMLStr, filename, fileExt drop 1).toString replaceAllLiterally("\\", "/"))
      } fold ((ExpectationFailed(_)), (url => Redirect("/" + url)))
  }

}
