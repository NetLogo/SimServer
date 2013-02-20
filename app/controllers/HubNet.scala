package controllers

import play.api.{ Logger, mvc}, mvc._

import scalaz.{ Scalaz, ValidationNEL }, Scalaz.ToValidationV

import models.filemanager.TempFileManager
import models.hubnet.{ HubNetServerManager, StudentInfo, TeacherInfo }
import models.jnlp._
import models.util.{ DecryptionUtil, EncryptionUtil, HubNetSettings, NetUtil, PBEWithMF5AndDES, ResourceManager, Util }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

  val HubNetKy     = "hubnet_data"
  val ModelsSubDir = "assets/models"
  val DepsSubDir   = "assets/deps"

  TempFileManager.removeAll()  // Clear all temp gen files on startup

  def hubTest = Action {
    Ok(views.html.hubtest(StudentInfo.form))
  }

  def bindStudent = Action {
    implicit request => StudentInfo.form.bindFromRequest.fold(
      errors => Ok(views.html.hubtest(errors)),
      {
        case StudentInfo(userName, teacherName) =>
          import HubNetSettings._
          val vals  = Seq(userName,    teacherName)
          val keys  = Seq(UserNameKey, TeacherNameKey)
          val pairs = keys zip vals
          val encryptedMaybe = encryptHubNetInfoPairs(Map(pairs: _*))
          handleHubNet(encryptedMaybe, false)
      }
    )
  }

  def hubTeach = Action {
    Ok(views.html.hubteach(TeacherInfo.form))
  }

  def bindTeacher = Action {
    implicit request => TeacherInfo.form.bindFromRequest.fold(
      errors => Ok(views.html.hubteach(errors)),
      {
        case TeacherInfo(modelName, userName, isHeadless, teacherName, portNumber, isLogging) =>
          import HubNetSettings._
          val vals  = Seq(modelName,    userName,    teacherName,    portNumber, isHeadless,    isLogging)
          val keys  = Seq(ModelNameKey, UserNameKey, TeacherNameKey, PortNumKey, IsHeadlessKey, IsLoggingKey)
          val pairs = keys zip vals
          val encryptedMaybe = encryptHubNetInfoPairs(Map(pairs: _*))
          encryptedMaybe fold ((nel => ExpectationFailed(nel.list.mkString("\n"))), (str => Redirect(routes.HubNet.hubSnoop(NetUtil.encode(str)))))
          // Fail or redirect to snoop the IP
      }
    )
  }

  private def encryptHubNetInfoPairs(requiredInfo: Map[String, String], optionalPairs: Option[(String, String)]*) : ValidationNEL[String, String] = {
    try {
      val kvMap = requiredInfo ++ optionalPairs.flatten
      val delimed = kvMap.toSeq map { case (k, v) => s"$k=$v" } mkString ResourceManager(ResourceManager.HubnetDelim)
      val encrypted = (new EncryptionUtil(ResourceManager(ResourceManager.HubNetKeyPass)) with PBEWithMF5AndDES) encrypt(delimed)
      encrypted.successNel[String]
    }
    catch {
      case ex: Exception =>
        val errorStr = "Failed to encrypt HubNet info"
        Logger.warn(errorStr, ex)
        s"$errorStr; ${ex.getMessage}".failNel
    }
  }

  def hubSnoop(encryptedInfo: String) = Action {
    Ok(views.html.hubsnoop(NetUtil.encode(encryptedInfo)))
  }

  def handleTeacherProxy(encryptedStr: String, teacherIP: String) = Action {
    request => handleHubNet(encryptedStr.successNel[String], true, Option(teacherIP))(request)
  }

  def handleHubNet(encryptedStrMaybe: ValidationNEL[String, String], isTeacher: Boolean, teacherIP: Option[String] = None)
                  (implicit request: Request[AnyContent]) : Result = {

    val inputAndSettingsMaybe =
      for (
        encryptedStr <- encryptedStrMaybe;
        settings     <- DecryptionUtil.decodeForHubNet(encryptedStr, isTeacher)
      ) yield (encryptedStr, settings)

    inputAndSettingsMaybe flatMap {
      case (input, HubNetSettings(modelNameOpt, username, isHeadless, teacherName, preferredPortOpt, isLogging)) =>

        val ipPortMaybe = {
          import HubNetServerManager._
          if (isTeacher) registerTeacherIPAndPort(teacherName, teacherIP.get, preferredPortOpt)
          else           getPortByTeacherName(teacherName)
        }

        val connectPath = s"http://${request.host}/logging"
        val programName = modelNameOpt getOrElse "NetLogo"
        val roleStr     = if (isTeacher) "Server" else "Client"

        // Start setting values to go into JSON
        import HubNetJNLP.{ generateAppName, generateDesc, generateShortDesc }

        val appName          = generateAppName(programName, roleStr)
        val desc             = generateDesc(programName, roleStr.toLowerCase)
        val shortDesc        = generateShortDesc(programName)
        val isOfflineAllowed = false
        val args = {
          import HubNetJNLP.{ generateIPArgs, generatePortArgs, generateUserIDArgs }, NetLogoJNLP.generateLoggingArgs
          if (isTeacher)
            ipPortMaybe.fold({_ => Seq()}, { case (_, port) => generatePortArgs(port) }) ++ generateLoggingArgs(isLogging)
          else
            ipPortMaybe map {
              case (ip, port) => generateUserIDArgs(username) ++ generateIPArgs(ip) ++ generatePortArgs(port)
            } getOrElse Seq()
        }

        // Building role-specific JSON
        import play.api.libs.json.Json.{ toJson => js }

        def propertyToJSON(property: (String, String)) = js(
          Map(
            JNLPKeys.PropertiesArrElemNameKey  -> js(property._1),
            JNLPKeys.PropertiesArrElemValueKey -> js(property._2)
          )
        )

        def jarToJSON(jar: Jar) = js(
          Map(
            JNLPKeys.OtherJarsArrElemNameKey   -> js(jar.jarName),
            JNLPKeys.OtherJarsArrElemIsLazyKey -> js(jar.isLazy)
          )
        )

        val properties        = Util.ifFirstWrapSecond(isLogging, ("jnlp.connectpath", connectPath)).toSeq
        val otherJars         = Seq()
        val roleExtrasMaybe   = {
          if (!isTeacher) Map(HubNetKeys.IsHubNetClientKey -> js(true)).successNel[String]
          else            modelNameOpt map (Models.getHubNetModelURL(_)) map {
            modelURL =>
              Map(
                HubNetKeys.IsHubNetServerKey -> js(true),
                NetLogoKeys.ModelURLKey      -> js(modelURL),
                JNLPKeys.PropertiesKey       -> js(properties map propertyToJSON),
                JNLPKeys.OtherJarsKey        -> js(otherJars  map jarToJSON)
              ).successNel[String]
          } getOrElse "No model name supplied".failNel
        }

        // Building general JSON; generating JNLP
        roleExtrasMaybe flatMap {
          extras =>
            val json = js(
              Map(
                JNLPKeys.ApplicationNameKey  -> js(appName),
                JNLPKeys.DescKey             -> js(desc),
                JNLPKeys.ShortDescKey        -> js(shortDesc),
                JNLPKeys.IsOfflineAllowedKey -> js(isOfflineAllowed),
                JNLPKeys.ArgumentsKey        -> js(args)
              ) ++ extras
            )
            JNLPFromJSONGenerator(json, request.host)
        }

    } fold ((nel => ExpectationFailed(nel.list.mkString("\n"))), (url => Redirect("/" + url)))

  }

  def javascriptRoutes() = Action {
    implicit request =>
    Ok(
      play.api.Routes.javascriptRouter("jsRoutes")(
        routes.javascript.HubNet.handleTeacherProxy
      )
    ).as("text/javascript")
  }

}
