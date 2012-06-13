package controllers

import play.api.mvc._
import play.api.Logger

import java.net.URI

import scalaz.{Validation, Failure, Success}

import models.hubnet.{StudentInfo, TeacherInfo, HubNetServerManager}
import models.jnlp.{Jar, MainJar, JNLP}
import models.util._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

  val CharEncoding = "UTF-8"
  val ModelsSubDir = "assets/misc/models"
  val DepsSubDir   = "assets/misc/deps"

  private lazy val thisIP = java.net.InetAddress.getLocalHost.getHostAddress

  TempGenManager.removeAll()  // Clear all temp gen files on startup

  //@ For testing only (maybe?)
  def hubTest = Action {
    Ok(views.html.hubtest(StudentInfo.form))
  }

  //@ For testing only (maybe)
  def bindStudent = Action {
    implicit request => StudentInfo.form.bindFromRequest.fold(
      errors => ExpectationFailed("Failed to parse form: " + errors.toString),
      {
        case StudentInfo(userName, teacherName) =>
          import HubNetSettings._
          val vals  = Seq(userName,    teacherName)
          val keys  = Seq(UserNameKey, TeacherNameKey)
          val pairs = vals zip keys
          val encryptedMaybe = encryptHubNetInfoPairs(Map(pairs: _*))
          handleHubNet(encryptedMaybe, false)
      }
    )
  }

  //@ For testing only
  def hubTeach = Action {
    Ok(views.html.hubteach(TeacherInfo.form))
  }

  //@ For testing only (maybe?)
  def bindTeacher = Action {
    implicit request => TeacherInfo.form.bindFromRequest.fold(
      errors => ExpectationFailed("Failed to parse form: " + errors.toString),
      {
        case TeacherInfo(modelName, userName, isHeadless, teacherName, portNumber, isLogging) =>
          import HubNetSettings._
          val vals  = Seq(modelName,    userName,    teacherName,    portNumber, isHeadless,    isLogging)
          val keys  = Seq(ModelNameKey, UserNameKey, TeacherNameKey, PortNumKey, IsHeadlessKey, IsLoggingKey)
          val pairs = keys zip vals
          val encryptedMaybe = encryptHubNetInfoPairs(Map(pairs: _*))
          encryptedMaybe fold ((ExpectationFailed(_)), (str => Redirect(routes.HubNet.hubSnoop(encode(str)))))
          // Fail or redirect to snoop the IP
      }
    )
  }

  private def encryptHubNetInfoPairs(requiredInfo: Map[String, String], optionalPairs: Option[(String, String)]*) : Validation[String, String] = {
    try {
      val kvMap = requiredInfo ++ optionalPairs.flatten
      val delimed = kvMap.toSeq map { case (k, v) => "%s=%s".format(k, v) } mkString ResourceManager(ResourceManager.HubnetDelim)
      val encrypted = (new EncryptionUtil(ResourceManager(ResourceManager.HubNetKeyPass)) with PBEWithMF5AndDES) encrypt(delimed)
      Success(encrypted)
    }
    catch {
      case e =>
        Logger.error(e.getLocalizedMessage)
        Failure("Failed to encrypt HubNet info; " + e.getLocalizedMessage)
    }
  }

  //@ Not currently used....  Should be used for optional parameters in conjunction with `encryptHubNetInfoPairs`
  private def morphPair2Opt(pair: Pair[String, String]) : Option[(String, String)] = {
    pair match {
      case (str, key) =>
        (if (!str.isEmpty) Option(str) else None) map {
          x =>      if (x == "Yes") "true"
               else if (x == "No")  "false"
               else                 x
        } map (key -> _)
    }
  }

  def hubSnoop(encryptedInfo: String) = Action {
    Ok(views.html.hubsnoop(encode(encryptedInfo)))
  }

  // Encoding and decoding is working properly here (which isn't all that surprising to me)
  def handleTeacherProxy(encryptedStr: String, teacherIP: String) = Action {
    request => handleHubNet(Success(encryptedStr), true, Option(teacherIP))(request)
  }

  def handleHubNet(encryptedStrMaybe: Validation[String, String], isTeacher: Boolean, teacherIP: Option[String] = None)
                  (implicit request: Request[AnyContent]) : Result = {

    // Could this benefit from using a `for` comprehension?
    val inputAndSettingsMaybe = encryptedStrMaybe flatMap (input => DecryptionUtil.decodeForHubNet(input) map (settings => (input, settings)))

    inputAndSettingsMaybe flatMap {
      case (input, HubNetSettings(modelNameOpt, username, isHeadless, teacherName, preferredPortOpt, isLogging)) =>

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

        val codebaseURL = routes.Assets.at("").absoluteURL(false) dropRight 1  // URL of 'assets'/'public' folder (drop the '/' from the end)
        val programName = modelNameOpt getOrElse "NetLogo"
        val fileName = TempGenManager.formatFilePath(input, "jnlp")
        val clientOrServerStr = if (!isHeadless && isTeacher) "Server" else "Client"
        val (mainClass, argsMaybe) = {
          if (isTeacher && !isHeadless)
            ("org.nlogo.app.App", modelNameOpt map {
                                    modelName => Seq("--url", Models.getModelURL(modelName)) ++
                                                 ipPortMaybe.fold( {_ => Seq()}, { case (_, port) => Seq("--port", port.toString)} ) ++
                                                 (if (isLogging) Seq("--logging") else Seq())
                                  } map (Success(_)) getOrElse Failure("No model name supplied."))
          else
            ("org.nlogo.hubnet.client.App", ipPortMaybe map { case (ip, port) => Seq("--id", username, "--ip", ip, "--port", port.toString) })
        }

        val propsMaybe = argsMaybe map {
          args => JNLP(
            codebaseURI      = new URI(codebaseURL),
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

  def javascriptRoutes() = Action {
    implicit request =>
    Ok(
      play.api.Routes.javascriptRouter("jsRoutes")(
        routes.javascript.HubNet.handleTeacherProxy
      )
    ).as("text/javascript")
  }

  //@ Does this really belong here?
  private def encode(str: String) : String = java.net.URLEncoder.encode(str, CharEncoding)

}
