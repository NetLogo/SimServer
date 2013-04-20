package controllers

import
  play.api.mvc._

import
  scalaz.Scalaz,
    Scalaz.ToValidationV

import
  models.{ hubnet, jnlp, util },
    hubnet.{ HubNetServerManager, StudentInfo, TeacherInfo },
    jnlp._,
    util.{ HubNetSettings, Util }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

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
          handleHubNet(pairs.toMap, false)
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
          handleHubNet(pairs.toMap, true)
      }
    )
  }

  private def handleHubNet(params: Map[String, String], isTeacher: Boolean)(implicit request: Request[AnyContent]) : Result = {

    val HubNetDefaultPort = 9173

    val settingsMaybe = HubNetSettings(params, isTeacher) map (_.successNel[String]) getOrElse "Failed to interpret settings".failNel

    settingsMaybe flatMap {
      case HubNetSettings(modelNameOpt, username, isHeadless, teacherName, preferredPortOpt, isLogging) =>

        val ipPortMaybe = {
          if (isTeacher)
            ("", preferredPortOpt getOrElse HubNetDefaultPort).successNel
          else
            HubNetServerManager.getPortByTeacherName(teacherName)
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

}
