package models.hubnet

import
  java.util.concurrent.TimeoutException

import
  scala.concurrent.{ Await, duration },
    duration._

import
  akka.{ actor, pattern, util => util_akka },
    actor._,
    pattern.ask,
    util_akka.Timeout

import
  play.{ api, libs },
    api.Logger,
    libs.Akka

import play.api.libs.concurrent.Execution.Implicits.defaultContext

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/21/13
 * Time: 8:25 PM
 */

class ExpiryManager[T](expireFunc: (T) => Unit, name: String) {

  protected val ActorPrefix = s"$name-expiry-"

  protected lazy val system = ActorSystem(name)

  protected object Messages {
    case object Expire
    case object Init
    case object Ping
    case object Pang
    case object Pong
    case object Refresh
  }

  import Messages._

  protected class ExpiryActor(id: T) extends Actor {

    protected val LifeSpan = 8 hours

    private var task = new Cancellable {
      override def cancel() {}
      override def isCancelled = true
    }

    override def receive = {
      case Init    => task = Akka.system.scheduler.scheduleOnce(LifeSpan) { self ! Expire }
      case Refresh => task.cancel();  self ! Init
      case Expire  => expireFunc(id); self ! PoisonPill
      case Ping    => sender ! Pong
    }

  }

  def apply(entryKey: T) {

    implicit val timeout = Timeout(1500 millis)

    val actor    = system.actorFor(s"/user/${generateActorPath(entryKey)}")
    val response = {
      try Await.result(actor ? Ping, timeout.duration)
      catch {
        case ex: TimeoutException => Pang
      }
    }

    response match {
      case Pong => actor ! Refresh
      case Pang => initExpiry(entryKey)
    }

  }

  private def initExpiry(entryKey: T) {

    val actorOpt = {
      try Option(system.actorOf(Props(new ExpiryActor(entryKey)), name = generateActorPath(entryKey)))
      catch {
        case ex: InvalidActorNameException =>
          Logger.warn("Actor name exception", ex)
          None
      }
    }

    actorOpt foreach (_ ! Init)

  }

  private def generateActorPath(entryKey: T) = s"${ActorPrefix}${entryKey.toString}"

}

