package controllers

import play.api.mvc._
import play.api.Logger

import java.net.URI

import scalaz.{ Failure, Success, Validation }

import models.hubnet.{ HubNetServerManager, StudentInfo, TeacherInfo }
import models.jnlp.{ HubNetJarManager, HubNetJNLP, Jar }
import models.filemanager.TempFileManager
import models.util.{ DecryptionUtil, EncryptionUtil, HubNetSettings, NetUtil, PBEWithMF5AndDES, ResourceManager, Util }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 4:20 PM
 */

object HubNet extends Controller {

  val HubNetKy     = "hubnet_data"
  val ModelsSubDir = "assets/misc/models"
  val DepsSubDir   = "assets/misc/deps"

  private lazy val thisIP = java.net.InetAddress.getLocalHost.getHostAddress

  TempFileManager.removeAll()  // Clear all temp gen files on startup

  def hubTest = Action {
    Ok(views.html.hubtest(StudentInfo.form))
  }

  def bindStudent = Action {
    implicit request => StudentInfo.form.bindFromRequest.fold(
      errors => Ok(views.html.hubtest(errors)),  //@ Is this really `bindStudents`'s business?
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
      errors => Ok(views.html.hubteach(errors)),  //@ Is this really `bindTeacher`'s business?
      {
        case TeacherInfo(modelName, userName, isHeadless, teacherName, portNumber, isLogging) =>
          import HubNetSettings._
          val vals  = Seq(modelName,    userName,    teacherName,    portNumber, isHeadless,    isLogging)
          val keys  = Seq(ModelNameKey, UserNameKey, TeacherNameKey, PortNumKey, IsHeadlessKey, IsLoggingKey)
          val pairs = keys zip vals
          val encryptedMaybe = encryptHubNetInfoPairs(Map(pairs: _*))
          encryptedMaybe fold ((ExpectationFailed(_)), (str => Redirect(routes.HubNet.hubSnoop(NetUtil.encode(str)))))
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
      case ex: Exception =>
        val errorStr = "Failed to encrypt HubNet info"
        Logger.warn(errorStr, ex)
        Failure("%s; %s".format(errorStr, ex.getMessage))
    }
  }

  //@ Not currently used....  Should be used for optional parameters in conjunction with `encryptHubNetInfoPairs`
  private def morphPair2Opt(pair: Pair[String, String]) : Option[(String, String)] = {
    pair match {
      case (str, key) =>
        Util.noneIfEmpty(str) map {
          x =>      if (x == "Yes") "true"
               else if (x == "No")  "false"
               else                 x
        } map (key -> _)
    }
  }

  def hubSnoop(encryptedInfo: String) = Action {
    Ok(views.html.hubsnoop(NetUtil.encode(encryptedInfo)))
  }

  def handleTeacherProxy(encryptedStr: String, teacherIP: String) = Action {
    request => handleHubNet(Success(encryptedStr), true, Option(teacherIP))(request)
  }

  def handleHubNet(encryptedStrMaybe: Validation[String, String], isTeacher: Boolean, teacherIP: Option[String] = None)
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
          if (isTeacher) {
            if (isHeadless)
              startUpServer(modelNameOpt, teacherName, thisIP)
            else
              registerTeacherIPAndPort(teacherName, teacherIP.get, preferredPortOpt)
          }
          else
            getPortByTeacherName(teacherName)
        }

        val JNLPConnectPath = "http://abmplus.tech.northwestern.edu:9001/logging"

        val codebaseURL = routes.Assets.at("").absoluteURL(false) dropRight 1  // URL of 'assets'/'public' folder (drop the '/' from the end)
        val programName = modelNameOpt getOrElse "NetLogo"
        val fileName = TempFileManager.formatFilePath(input, "jnlp")
        val clientOrServerStr = if (!isHeadless && isTeacher) "Server" else "Client" //@ This logic should go to a JNLP class

        val (mainClass, jvmArgs, argsMaybe) = {
          import HubNetJNLP.{ generateIPArgs, generateModelURLArgs, generatePortArgs, generateUserIDArgs }
          import HubNetJarManager._
          if (isTeacher && !isHeadless) {
            val args =
              modelNameOpt map {
                modelName => generateModelURLArgs(Models.getHubNetModelURL(modelName)) ++
                             ipPortMaybe.fold( {_ => Seq()}, { case (_, port) => generatePortArgs(port)} ) ++
                             (Util.ifFirstWrapSecond(isLogging, "--logging").toSeq) //@ This "--logging" should get moved out to a JNLP class
              } map (Success(_)) getOrElse Failure("No model name supplied")
            (ServerMainClass, ServerVMArgs, args)
          }
          else
            (ClientMainClass, ClientVMArgs, ipPortMaybe map {
              case (ip, port) => generateUserIDArgs(username) ++ generateIPArgs(ip) ++ generatePortArgs(port)
            })
        }

        val properties = Util.ifFirstWrapSecond(isLogging, ("jnlp.connectpath", JNLPConnectPath)).toSeq
        val otherJars  = Util.ifFirstWrapSecond(isLogging, new Jar("logging.jar", true)).toSeq

        val propsMaybe = argsMaybe map {
          args =>
            new HubNetJNLP(
              codebaseURI       = new URI(codebaseURL),
              jnlpLoc           = fileName,
              mainClass         = mainClass,
              programName       = programName,
              roleStr           = clientOrServerStr,
              isOfflineAllowed  = false,
              vmArgs            = jvmArgs,
              otherJars         = otherJars,
              properties        = properties,
              args              = args
            )
        }

        propsMaybe map (jnlp => TempFileManager.registerFile(jnlp.toXMLStr, fileName).toString replaceAllLiterally("\\", "/"))

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

}
