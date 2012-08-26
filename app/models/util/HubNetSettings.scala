package models.util

import java.util.InputMismatchException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/30/12
 * Time: 5:15 PM
 */

sealed abstract case class HubNetSettings(modelNameOpt: Option[String], userName: String, isHeadless: Boolean,
                                          teacherName: String, desiredPortNumOpt: Option[Int], isLogging: Boolean)

sealed class StudentHubNetSettings(uname: String, tname: String) extends HubNetSettings(None, uname, false, tname, None, false)

sealed class TeacherHubNetSettings(modelName: Option[String], uname: String, headless: Boolean,
                                   tname: String, portNum: Option[Int], logging: Boolean)
                                   extends HubNetSettings(modelName, uname, headless, tname, portNum, logging)

object HubNetSettings {

  val ModelNameKey   = "model_name"
  val UserNameKey    = "username"
  val TeacherNameKey = "teacher_name"
  val PortNumKey     = "port_num"
  val IsHeadlessKey  = "is_headless"
  val IsLoggingKey   = "is_logging"

  // Could return a `Validation`, but I don't think that my use of `Validation` is this class's business
  def apply(inMap: Map[String, String], isTeacher: Boolean) : Option[HubNetSettings] = {

    // These are all `Option`s
    val (modelName, userName, isHeadless, teacherName, portNum, isLogging) = {
      def defaultOnAndWrapBoolStr(strOpt: Option[String]) = Option(strOpt map {
        case "Yes" => "true"
        case "No"  => "false"
        case x     => x
      } map (_.toBoolean) getOrElse false)
      import inMap.get
      (get(ModelNameKey), get(UserNameKey), defaultOnAndWrapBoolStr(get(IsHeadlessKey)),
       get(TeacherNameKey), get(PortNumKey) map (_.toInt), defaultOnAndWrapBoolStr(get(IsLoggingKey)))
    }

    // If something needs to not be `None`, add it here
    val questionables = if (isTeacher) List(userName, isHeadless, teacherName, isLogging) else List(userName, teacherName)
    val verifieds = if (questionables forall (!_.isEmpty)) Option(questionables.flatten) else None

    verifieds map {
      case (uname: String) :: (headless: Boolean) :: (tname: String) :: (logging: Boolean) :: Nil =>
        new TeacherHubNetSettings(modelName, uname, headless, tname, portNum, logging)
      case (uname: String) :: (tname: String) :: Nil =>
        new StudentHubNetSettings(uname, tname)
      case _ =>
        throw new InputMismatchException("The `HubNetSettings` factory could not generate an instance with the given data pattern.")
    }

  }
  
}
