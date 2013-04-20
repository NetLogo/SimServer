package models.hubnet

import
  scala.collection.mutable.{ Map => MMap }

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz.ToValidationV

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object HubNetServerManager {

  private val NotStartedFormat = "There is no existing HubNet server for teacher %s.  Please ask your teacher to connect to the activity and then try again.\n".format((_: String))

  private val teacherToIPPortMap = MMap[String, (String, Int)]()

  def getPortByTeacherName(teacherName: String) : ValidationNel[String, (String, Int)] =
    teacherToIPPortMap.get(teacherName) map (_.successNel[String]) getOrElse NotStartedFormat(teacherName).failNel

}
