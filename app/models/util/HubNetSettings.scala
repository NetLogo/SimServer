package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/30/12
 * Time: 5:15 PM
 */

class HubNetSettings(val modelNameOpt: Option[String], val userName: String, val isHeadless: Boolean,
                     val teacherName: String, val isTeacher: Boolean, val desiredPortNumOpt: Option[Int],
                     val isLogging: Boolean)

object HubNetSettings {

  val ModelNameKey   = "model_name"
  val UserNameKey    = "username"
  val TeacherNameKey = "teacher_name"
  val PortNumKey     = "port_num"
  val IsHeadlessKey  = "is_headless"
  val IsTeacherKey   = "is_teacher"
  val IsLoggingKey   = "is_logging"

  def unapply(settings: HubNetSettings) : Option[(Option[String], String, Boolean, String, Boolean, Option[Int], Boolean)] = {
    import settings._; Option(modelNameOpt, userName, isHeadless, teacherName, isTeacher, desiredPortNumOpt, isLogging)
  }

  // Could return a `Validation`, but I don't think that my use of `Validation` is this class's business
  def apply(inMap: Map[String, String]) : Option[HubNetSettings] = {

    // These are all `Option`s
    val (modelName, userName, isHeadless, teacherName, isTeacher, portNum, isLogging) = {
      def defaultOnAndWrapBoolStr(strOpt: Option[String]) = Option(strOpt map (_.toBoolean) getOrElse false)
      import inMap.get
      (get(ModelNameKey), get(UserNameKey), defaultOnAndWrapBoolStr(get(IsHeadlessKey)),
       get(TeacherNameKey), defaultOnAndWrapBoolStr(get(IsTeacherKey)), get(PortNumKey) map (_.toInt),
       defaultOnAndWrapBoolStr(get(IsLoggingKey)))
    }

    // If something needs to not be `None`, add it here
    val questionables = List(userName, isHeadless, teacherName, isTeacher, isLogging)
    val verifieds = if (questionables forall (!_.isEmpty)) Option(questionables.flatten) else None

    verifieds map {
      case (uname: String) :: (headless: Boolean) :: (tname: String) :: (teacher: Boolean) :: (logging: Boolean) :: Nil =>
        new HubNetSettings(modelName, uname, headless, tname, teacher, portNum, logging)
    }

  }
  
}
