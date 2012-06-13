package models.hubnet

import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.Await
import akka.pattern.ask
import scalaz.{Failure, Success, Validation}
import models.{ServerStatus, Start, Started}
import akka.actor.ActorSystem._
import akka.actor.{Props, ActorSystem}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object HubNetServerManager {

  private val NotStartedFormat = "There is no existing HubNet server for teacher %s.  Please ask your teacher to connect to the activity and then try again.\n".format((_: String))
  private val StartingPort = 9173
  private val ServerCountLimit = 20

  private val system = ActorSystem("HeadlessServers")

  private val portServerMap = (Stream continually (system.actorOf(Props(new HubNetServerActor))) take ServerCountLimit zipWithIndex).
                               map { case (actor, offset) => (StartingPort + offset, actor) } toMap
  
  private val teacherToIPPortMap = collection.mutable.Map[String, (String, Int)]()

  def startUpServer(modelNameOpt: Option[String], teacherName: String, ip: String) : Validation[String, (String, Int)] = {

    val modelNameMaybe = modelNameOpt map (Success(_)) getOrElse Failure("No model name given\n")

    val portMaybeFunc = (m: String) =>
      portServerMap find {
        case (port, server) =>
          implicit val timeout = Timeout(3 seconds)
          Await.result(server ? Start(m, port), timeout.duration).asInstanceOf[ServerStatus] == Started
      } map {
        case (port, server) =>
          registerTeacherIPAndPort(teacherName, ip, Option(port))
          Success(port)
      } getOrElse Failure("Unable to start any servers at the moment.\n")

    modelNameMaybe flatMap portMaybeFunc map ((ip, _))

  }

  def registerTeacherIPAndPort(teacherName: String, ip: String, portOpt: Option[Int] = None) : Success[String, (String, Int)] = {
    val port = portOpt getOrElse StartingPort
    teacherToIPPortMap += teacherName -> (ip, port)
    Success(ip, port)
  }

  def getPortByTeacherName(teacherName: String) : Validation[String, (String, Int)] = {
    teacherToIPPortMap.get(teacherName) map { case (ip, port) => Success((ip, port)) } getOrElse Failure(NotStartedFormat(teacherName))
  }

}
