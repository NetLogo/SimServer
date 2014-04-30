package models.hubnet

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz.ToValidationV

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object UnsafeHubNetServerRegistry extends CanManageHubNetRegistries {

  override protected type RegistryValue      = Option[LookupAddress]
  override protected type RegistrationReturn = Unit

  override def registerTeacher(teacherName: String): Unit = {
    registryMap += teacherName -> None
  }

  override def registerLookupAddress(teacherName: String, data: String): Unit = {
    val Seq(ip, port) = data.split(":").toSeq
    registryMap += teacherName -> Option(LookupAddress(ip, port.toInt))
    expiryManager(teacherName)
  }

  override def getIPAndPortByTeacherName(teacherName: String): ValidationNel[String, (String, Int)] =
    registryMap.get(teacherName) flatMap {
      addrOpt => // If the teacher has at least started registration...
        Option(
          addrOpt map {
            case LookupAddress(ip, port) => (ip, port).successNel[String] // Use the registry entry if it exists
          } getOrElse (
            NotStartedFormat(teacherName).failNel // Otherwise say that the teacher hasn't finished registration yet
          )
        )
    } getOrElse (
      NotFoundFormat(teacherName).failNel // Otherwise say that the teacher has not yet registered at all
    )

}

