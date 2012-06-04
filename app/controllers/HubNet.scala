package controllers

import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import java.net.URI
import models.hubnet.HubNetServerManager
import models.util._
import scalaz.{Validation, Failure, Success}
import models.jnlp.{Jar, MainJar, JNLP}

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

  //@ For testing only
  def form = Form(
    tuple(
      "Model Name"   -> text,
      "User Name"    -> text,
      "Is Headless"  -> text,
      "Teacher Name" -> text,
      "Is Teacher"   -> text,
      "Port Number"  -> text,
      "Is Logging"   -> text
    )
  )

  //@ For testing only
  def hubTest = Action {
    Ok(views.html.hubtest(form))
  }

  //@ For testing only
  def hubData = Action {
    implicit request => form.bindFromRequest.fold(
      errors => Ok(views.html.hubdata("Nice failure", "You suck")),
      {
        case (modelName, userName, isHeadless, teacherName, isTeacher, portNumber, isLogging) =>

          import HubNetSettings._

          def morphBlnStr2OZ(blnStr: String, key: String) : Seq[(String, String)] = {
            if (blnStr != "N/A") Seq(key -> (if (blnStr == "Yes") "true" else "false")) else Seq()
          }

          val ip = java.net.InetAddress.getLocalHost.getHostAddress

          val modelOZ    = if (!modelName.isEmpty)  Seq(ModelNameKey -> modelName) else Seq()
          val portNumOZ  = if (!portNumber.isEmpty) Seq(PortNumKey -> portNumber)  else Seq()
          val teachIPOZ  = if (!ip.isEmpty)  Seq(TeacherIPKey -> ip) else Seq()
          val headlessOZ = morphBlnStr2OZ(isHeadless, IsHeadlessKey)
          val teacherOZ  = morphBlnStr2OZ(isTeacher,  IsTeacherKey)
          val loggingOZ  = morphBlnStr2OZ(isLogging,  IsLoggingKey)

          val kvMap   = Map(UserNameKey -> userName, TeacherNameKey -> teacherName) ++ loggingOZ ++ teacherOZ ++ portNumOZ ++ headlessOZ ++ modelOZ ++ teachIPOZ
          val delimed = kvMap.toSeq map { case (k, v) => "%s=%s".format(k, v) } mkString ResourceManager(ResourceManager.HubnetDelim)
          val encrypted = (new EncryptionUtil(ResourceManager(ResourceManager.HubNetKeyPass)) with PBEWithMF5AndDES) encrypt(delimed)

          reallyHandleHubNet(Success(encrypted), request)

      }
    )
  }

  def handleHubNet = Action {
    request =>
    val inputMaybe = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                             orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                             orElse(Option(request.queryString)).
                             flatMap(_.get(HubNetKey)).flatMap(_.headOption) map (Success(_)) getOrElse (Failure("Invalid POST data"))
    reallyHandleHubNet(inputMaybe, request)
  }

  def reallyHandleHubNet(encryptedStrMaybe: Validation[String, String], request: Request[AnyContent]) : Result = {

      val inputAndSettingsMaybe = encryptedStrMaybe flatMap (input => DecryptionUtil.decodeForHubNet(input) map (settings => (input, settings)))

      inputAndSettingsMaybe flatMap {
        case (input, HubNetSettings(modelNameOpt, username, isHeadless, teacherName, isTeacher, preferredPortOpt, isLogging, teacherIP)) =>

          //val clientIP = "129.105.107.206" //@ We need to get this from somewhere (eventually)

          val ipPortMaybe = {
            import HubNetServerManager._
            if (isTeacher) {
              if (isHeadless)
                startUpServer(modelNameOpt, teacherName, thisIP)
              else
                registerTeacherIPAndPort(teacherName, teacherIP.get, preferredPortOpt)
            }
            else
              getPortByTeacherName(teacherName)
          }

          val host = "http://" + request.host
          val programName = modelNameOpt getOrElse "NetLogo"
          val fileName = TempGenManager.formatFilePath(input, "jnlp")
          val clientOrServerStr = if (!isHeadless && isTeacher) "Server" else "Client"
          val (mainClass, argsMaybe) = {
            if (isTeacher && !isHeadless)
              ("org.nlogo.app.App", modelNameOpt map
                                    (modelName => Seq("--url", ModelUtil.getURLFromName(modelName)) ++
                                                  ipPortMaybe.fold({_ => Seq()}, { case (_, port) => Seq("--port", port.toString) }) ++
                                                  (if (isLogging) Seq("--logging") else Seq())) map
                                    (Success(_)) getOrElse Failure("No model name supplied."))
            else
              ("org.nlogo.hubnet.client.App", ipPortMaybe map { case (ip, port) => Seq("--id", username, "--ip", ip, "--port", port.toString) })
          }

          val propsMaybe = argsMaybe map {
            args => JNLP(
              serverPublicURI  = new URI(host),
              jnlpLoc          = fileName,
              mainJar          = new MainJar("NetLogo.jar"),
              applicationName  = "%s HubNet %s".format(programName, clientOrServerStr),
              mainClass        = mainClass,
              appTitle         = "NetLogo HubNet %s".format(clientOrServerStr),
              desc             = "A HubNet %s for %s".format(clientOrServerStr.toLowerCase, programName),
              shortDesc        = "HubNet (%s)".format(programName),
              isOfflineAllowed = false,
              otherJars        = if (isLogging) Seq(new Jar("logging.jar", true)) else Seq(),
              properties       = if (isLogging) Seq(("jnlp.connectpath", "http://abmplus.tech.northwestern.edu:9001/logging")) else Seq(),
              arguments        = args
            )
          }

          propsMaybe map (jnlp => TempGenManager.registerFile(jnlp.toXMLStr, fileName).toString replaceAllLiterally("\\", "/"))

      } fold ((ExpectationFailed(_)), (url => Redirect("/" + url)))

  }

}
