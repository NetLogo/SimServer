package models.hubnet

import scalaz.{Failure, Success, Validation}

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

  def registerTeacherIPAndPort(teacherName: String, ip: String, portOpt: Option[Int] = None) : Success[String, (String, Int)] = {
    val port = portOpt getOrElse StartingPort
    teacherToIPPortMap += teacherName -> (ip, port)
    Success(ip, port)
  }

  def getPortByTeacherName(teacherName: String) : Validation[String, (String, Int)] =
    teacherToIPPortMap.get(teacherName) map { case (ip, port) => Success((ip, port)) } getOrElse Failure(NotStartedFormat(teacherName))

}
