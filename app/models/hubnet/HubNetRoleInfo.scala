package models.hubnet
import Converter.str2Option
import play.api.data.Forms._
import play.api.data.Form

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/7/12
 * Time: 1:28 PM
 */

private object Converter {
  implicit def str2Option(str: String) = Option(str)
}

sealed class HubNetRoleInfo(val modelName: Option[String] = None, val username: String, val isHeadless: Option[String] = None,
                            val teacherName: String, val portNum: Option[String] = None, val isLogging: Option[String] = None)

sealed trait InfoCompanion[T] {
  def form : Form[T]
}

case class StudentInfo(private val uname: String, private val tname: String) extends HubNetRoleInfo(username = uname, teacherName = tname)

object StudentInfo extends InfoCompanion[StudentInfo] {
  override def form : Form[StudentInfo] = {
    Form(
      mapping(
        "User Name"    -> text,
        "Teacher Name" -> text
      )(StudentInfo.apply)(StudentInfo.unapply)
    )
  }
}

case class TeacherInfo(private val mname: String, private val uname: String, private val headless: String,
                       private val tname: String, private val port: String,  private val logging: String)
     extends HubNetRoleInfo(mname, uname, headless, tname, port, logging) {

  def generateWithIP(ipAddr: String) : TeacherInfo = new TeacherInfo(mname, uname, headless, tname, port, logging)
}

object TeacherInfo extends InfoCompanion[TeacherInfo] {
  override def form : Form[TeacherInfo] = {
    Form(
      mapping(
        "Model Name"   -> text,
        "User Name"    -> text,
        "Is Headless"  -> text,
        "Teacher Name" -> text,
        "Port Number"  -> text,
        "Is Logging"   -> text
      )(TeacherInfo.apply)(TeacherInfo.unapply)
    )
  }
}
