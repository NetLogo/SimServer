package models.hubnet

import scalaz.{ Scalaz, ValidationNEL }, Scalaz.ToValidationV

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object HubNetServerManager {

  private val NotStartedFormat = "There is no existing HubNet server for teacher %s.  Please ask your teacher to connect to the activity and then try again.\n".format((_: String))
  private val StartingPort = 9173

  private val teacherToIPPortMap = collection.mutable.Map[String, (String, Int)]()

  def registerTeacherIPAndPort(teacherName: String, ip: String, portOpt: Option[Int] = None) : ValidationNEL[String, (String, Int)] = {
    val entry = (ip, portOpt getOrElse StartingPort)
    teacherToIPPortMap += teacherName -> entry
    entry.successNel[String]
  }

  def getPortByTeacherName(teacherName: String) : ValidationNEL[String, (String, Int)] =
    teacherToIPPortMap.get(teacherName) map (_.successNel[String]) getOrElse NotStartedFormat(teacherName).failNel

}
