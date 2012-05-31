package controllers

import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import java.net.URI
import models.jnlp.{MainJar, JNLP}
import models.hubnet.HubNetServerManager
import models.util.{HubNetSettings, TempGenManager, DecryptionUtil}
import scalaz.{Failure, Success}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

  val HubNetKey      = "hubnet_data"
  val ModelsSubDir   = "assets/misc/models"
  val DepsSubDir     = "assets/misc/deps"

  private lazy val thisIP = "129.105.107.206" //@ Eventually, do this properly

  TempGenManager.removeAll()  // Clear all temp gen files on startup

  def form = Form(
    tuple(
      "Model Name" -> text,
      "User Name" -> text,
      "Is Logging" -> text,
      "Teacher Name" -> text,
      "Is Logging" -> text,
      "Port Number" -> text,
      "Is Logging" -> text
    )
  )

  //@ For testing only
  def hubTest = Action {
    Ok(views.html.hubtest(form))
  }

  //@ For testing only
  def yourData = Action {
    implicit request => form.bindFromRequest.fold(
      errors => Ok(views.html.yourData("Nice failure", "You suck")),
      tuple  => Ok(views.html.yourData(tuple._1, tuple._2))
    )
  }

  def handleHubNet = Action {
    request =>

    val inputMaybe = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                             orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                             orElse(Option(request.queryString)).
                             flatMap(_.get(HubNetKey)).flatMap(_.headOption) map (Success(_)) getOrElse (Failure("Invalid POST data"))

    val inputAndSettingsMaybe = inputMaybe flatMap (input => DecryptionUtil.decodeForHubNet(input) map (settings => (input, settings)))

    inputAndSettingsMaybe flatMap {
      case (input, HubNetSettings(modelNameOpt, username, isHeadless, teacherName, isTeacher, preferredPortOpt, isLogging)) =>

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
        val fileName = TempGenManager.formatFilePath(input, "jnlp")
        val (mainClass, argsMaybe) = {
          if (isTeacher && !isHeadless)
            ("org.nlogo.app.App", modelNameOpt map (Seq(_) ++ (if (isLogging) Seq("--logging") else Seq())) map (Success(_)) getOrElse Failure("No model name supplied."))
          else
            ("org.nlogo.hubnet.client.App", ipPortMaybe map { case (ip, port) => Seq("--id", username, "--ip", ip, "--port", port.toString) })
        }

        val propsMaybe = argsMaybe map {
          args => JNLP(
            serverPublicURI  = new URI(host),
            jnlpLoc          = fileName,
            mainJar          = new MainJar("NetLogo.jar"),
            applicationName  = "%s HubNet Client".format(programName),
            mainClass        = mainClass,
            appTitle         = "NetLogo HubNet Client",
            desc             = "A HubNet client for %s".format(programName),
            shortDesc        = "HubNet (%s)".format(programName),
            isOfflineAllowed = false,
            properties       = if (isLogging) Seq(("jnlp.connectpath", "http://abmplus.tech.northwestern.edu:9001/logging")) else Seq(),
            arguments        = args
          )
        }

        propsMaybe map (jnlp => TempGenManager.registerFile(jnlp.toXMLStr, fileName).toString replaceAllLiterally("\\", "/"))

    } fold ((ExpectationFailed(_)), (url => Redirect("/" + url)))

  }

}
