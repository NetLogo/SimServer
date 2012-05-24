package models.hubnet

import models.{Start, Started}
import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.Await
import scalaz.{Failure, Success, Validation}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object HubNetServerManager {

  private val NotStartedFormat = "There is no existing HubNet server for teacher %s.  Please ask your teacher to connect to the activity and then try again.\n".format(_)
  private val StartingPort = 9173
  private val ServerCountLimit = 20

  private val portServerMap = (Stream continually (new HubNetServerActor) take ServerCountLimit zipWithIndex).
                               map { case (actor, offset) => (StartingPort + offset, actor) } toMap
  
  private val teacherPortMap = collection.mutable.Map[String, Int]()

  def startUpServer(modelNameOpt: Option[String], teacherName: String) : Validation[String, Int] = {
    val modelNameMaybe = modelNameOpt map (Success(_)) getOrElse Failure("No model name given\n")
    val portMaybeFunc = (m: String) => portServerMap.find { case (port, server) => Await.result((server ? Start(m)), Timeout(3 seconds).duration) == Started }. //@ May need `asInstanceOf[Boolean]
                                       map { case (port, server) => Success(port) } getOrElse Failure("Unable to start any servers.\n")
    modelNameMaybe flatMap (portMaybeFunc(_))
  }

  def getPortByTeacherName(teacherName: String) : Validation[String, Int] = {
    teacherPortMap.get(teacherName) map (Success(_)) getOrElse Failure(NotStartedFormat(teacherName))
  }

}
