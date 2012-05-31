package controllers

import play.api._
import play.api.mvc._
import models.log.LoggingHandler
import java.net.URI
import models.jnlp.{MainJar, JNLP}
import models.hubnet.HubNetServerManager
import models.util.{HubNetSettings, TempGenManager, DecryptionUtil, RequestUtil}
import scalaz.{Failure, Success}

object Application extends Controller {

  val LoggingDataKey = "logging_data"
  val HubNetKey      = "hubnet_data"
  val ModelsSubDir   = "assets/misc/models"
  val DepsSubDir     = "assets/misc/deps"

  private lazy val thisIP = "129.105.107.206" //@ Eventually, do this properly

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
                               flatMap(_.get(HubNetKey)).flatMap(_.headOption) map (Success(_)) getOrElse (Failure("Invalid POST data"))

      val inputAndSettingsMaybe = inputMaybe flatMap (input => DecryptionUtil.decodeForHubNet(input) map (settings => (input, settings)))

      inputAndSettingsMaybe flatMap {
        case (input, HubNetSettings(modelNameOpt, username, isHeadless, teacherName, isTeacher, preferredPortOpt)) =>

          val clientIP = "129.105.107.206" //@ We need to get this from somewhere (eventually)

          val ipPortMaybe = {
            import HubNetServerManager._
            if (isTeacher) {
              if (isHeadless)
                startUpServer(modelNameOpt, teacherName, thisIP)
              else
                registerTeacherIPAndPort(teacherName, clientIP, preferredPortOpt)
            }
            else
              getPortByTeacherName(teacherName)
          }

          val host = "http://" + request.host
          val programName = modelNameOpt getOrElse "NetLogo"
          val fileExt = "jnlp"
          val fileName = TempGenManager.formatFilePath(input, fileExt)
          val (mainClass, argsMaybe) = {
            if (isTeacher && !isHeadless)
              ("org.nlogo.app.App", Success(Seq()))
            else
              ("org.nlogo.hubnet.client.App", ipPortMaybe map { case (ip, port) => Seq("--id", username, "--ip", ip, "--port", port.toString) })
          }

          val propsMaybe = argsMaybe map {
            args => JNLP(
              new URI(host),
              fileName,
              new MainJar("NetLogo.jar"),
              "%s HubNet Client".format(programName),
              mainClass,
              "NetLogo HubNet Client",
              "A HubNet client for %s".format(programName),
              "HubNet (%s)".format(programName),
              false,
              arguments = args
            )
          }

          propsMaybe map (jnlp => TempGenManager.registerFile(jnlp.toXMLStr, fileName).toString replaceAllLiterally("\\", "/"))

      } fold ((ExpectationFailed(_)), (url => Redirect("/" + url)))

  }

}
