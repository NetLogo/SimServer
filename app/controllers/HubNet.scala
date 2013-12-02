package controllers

import
  org.apache.commons.codec.binary.Base64

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz.ToValidationV

import
  play.api.{ libs, mvc },
    libs.json.{ Json, JsValue },
    mvc.{ Action, AnyContent, Controller, Request, Result }

import
  controllers.action.APIAction

import
  models.{ hubnet, jnlp, util },
    hubnet.{ HubNetSettings, HubNetServerRegistry, StudentInfo, TeacherInfo },
    jnlp.{ HubNetJNLP, HubNetKeys, Jar, JNLPFromJSONGenerator, JNLPKeys, NetLogoKeys },
    util.PlayUtil

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

  def registerTeacherAddress = Action {
    request =>
      val bundle      = PlayUtil.extractBundle(request)
      val teacherName = bundle.stringParams(SecureJNLP.HTTPParams.TeacherNameKey)
      val data        = bundle.stringParams(SecureJNLP.HTTPParams.DataKey)
      val decodedData = Base64.decodeBase64(data.getBytes)
      HubNetServerRegistry.registerLookupAddress(teacherName, decodedData)
      Ok
  }

  def hubTest = Action {
    Ok(views.html.hubtest(StudentInfo.form))
  }

  def bindStudent = APIAction {
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

    import HubNetJNLP.{ generateAppName, generateDesc, generateIPArgs, generatePortArgs, generateShortDesc, generateUserIDArgs }

    val HubNetDefaultPort = 9173

    val settingsMaybe = HubNetSettings(params, isTeacher) map (_.successNel[String]) getOrElse "Failed to interpret settings".failNel

    settingsMaybe flatMap {
      case HubNetSettings(modelNameOpt, username, isHeadless, teacherName, preferredPortOpt, isLogging) =>

        val ipPortMaybe = {
          if (isTeacher)
            ("", preferredPortOpt getOrElse HubNetDefaultPort).successNel
          else
            HubNetServerRegistry.getPortByTeacherName(teacherName)
        }

        val programName = modelNameOpt getOrElse "NetLogo"
        val roleStr     = if (isTeacher) "Server" else "Client"

        val appName          = generateAppName(programName, roleStr)
        val desc             = generateDesc(programName, roleStr.toLowerCase)
        val shortDesc        = generateShortDesc(programName)
        val isOfflineAllowed = false
        val modelURLOpt      = modelNameOpt map (Models.getHubNetModelURL(_))
        val properties       = Seq()
        val otherJars        = Seq()

        val args = {
          if (isTeacher)
            ipPortMaybe.fold({_ => Seq()}, { case (_, port) => generatePortArgs(port) })
          else
            ipPortMaybe map {
              case (ip, port) => generateUserIDArgs(username) ++ generateIPArgs(ip) ++ generatePortArgs(port)
            } getOrElse Seq()
        }

        val jsonMaybe = paramsToJson(appName, desc, shortDesc, isOfflineAllowed, isTeacher, isLogging, modelURLOpt, args, properties, otherJars)
        jsonMaybe flatMap (JNLPFromJSONGenerator(_, request.host))

    } fold ((nel => ExpectationFailed(nel.list.mkString("\n"))), (url => Redirect("/" + url)))

  }

  private def paramsToJson(appName: String, desc: String, shortDesc: String, isOfflineAllowed: Boolean,  isTeacher: Boolean,
                           isLogging: Boolean, modelURLOpt: Option[String], args: Seq[String], properties: Seq[(String, String)],
                           otherJars: Seq[Jar]) : ValidationNel[String, JsValue] = {

    import Json.{ toJson => js }

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

    val roleExtrasMaybe = {
      if (!isTeacher)
        Map(HubNetKeys.IsHubNetClientKey -> js(true)).successNel[String]
      else
        modelURLOpt map {
          modelURL =>
            Map(
              HubNetKeys.IsHubNetServerKey -> js(true),
              NetLogoKeys.ModelURLKey      -> js(modelURL),
              NetLogoKeys.IsLoggingKey     -> js(isLogging),
              JNLPKeys.PropertiesKey       -> js(properties map propertyToJSON),
              JNLPKeys.OtherJarsKey        -> js(otherJars  map jarToJSON)
            ).successNel[String]
        } getOrElse "No model name supplied".failNel
    }

    roleExtrasMaybe map {
      extras =>
        js(
          Map(
            JNLPKeys.ApplicationNameKey  -> js(appName),
            JNLPKeys.DescKey             -> js(desc),
            JNLPKeys.ShortDescKey        -> js(shortDesc),
            JNLPKeys.IsOfflineAllowedKey -> js(isOfflineAllowed),
            JNLPKeys.ArgumentsKey        -> js(args)
          ) ++ extras
        )
    }

  }

}
