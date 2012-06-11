package models.util

import java.util.InputMismatchException

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/30/12
 * Time: 5:15 PM
 */

class HubNetSettings(val modelNameOpt: Option[String], val userName: String, val isHeadless: Boolean,
                     val teacherName: String, val desiredPortNumOpt: Option[Int], val isLogging: Boolean)

object HubNetSettings {

  val ModelNameKey   = "model_name"
  val UserNameKey    = "username"
  val TeacherNameKey = "teacher_name"
  val PortNumKey     = "port_num"
  val IsHeadlessKey  = "is_headless"
  val IsLoggingKey   = "is_logging"

  def unapply(settings: HubNetSettings) : Option[(Option[String], String, Boolean, String, Option[Int], Boolean)] = {
    import settings._; Option(modelNameOpt, userName, isHeadless, teacherName, desiredPortNumOpt, isLogging)
  }

  // Could return a `Validation`, but I don't think that my use of `Validation` is this class's business
  def apply(inMap: Map[String, String]) : Option[HubNetSettings] = {

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
    val questionables = List(userName, isHeadless, teacherName, isLogging)
    val verifieds = if (questionables forall (!_.isEmpty)) Option(questionables.flatten) else None

    verifieds map {
      case (uname: String) :: (headless: Boolean) :: (tname: String) :: (logging: Boolean) :: Nil =>
        new HubNetSettings(modelName, uname, headless, tname, portNum, logging)
      case _ => throw new InputMismatchException("You changed the contents of `questionables` in the `HubNetSettings` factory without changing the pattern matching!")
    }

  }
  
}
