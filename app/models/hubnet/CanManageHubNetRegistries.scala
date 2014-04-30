package models.hubnet

import
  scala.collection.mutable.{ Map => MMap }

import
  scalaz.ValidationNel

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/30/14
 * Time: 2:01 PM
 */

trait CanManageHubNetRegistries {

  protected type RegistryValue
  protected type RegistrationReturn

  protected val NotFoundFormat   = "Teacher '%s' has not attempted to start any HubNet servers recently.".format(_: String)
  protected val NotStartedFormat = "There is no existing HubNet server for teacher '%s'.  Please ask your teacher to connect to the activity and then try again.\n".format(_: String)

  protected val registryMap   = MMap[String, RegistryValue]()
  protected val expiryManager = new ExpiryManager(removeEntry, "hubnet-registry")

  def registerTeacher(teacherName: String): RegistrationReturn

  def registerLookupAddress(teacherName: String, data: String): Unit

  def getIPAndPortByTeacherName(teacherName: String): ValidationNel[String, (String, Int)]

  private def removeEntry(teacherName: String): Unit = {
    registryMap -= teacherName
  }

}

protected[hubnet] case class LookupAddress(ip: String, port: Int)
