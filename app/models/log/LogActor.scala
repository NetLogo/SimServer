package models.log

import
  scala.{ concurrent, io => sio, util => sutil },
    concurrent.duration._,
    sio.Source,
    sutil.control.Exception

import
  java.{ io => jio, text, util => jutil },
    jio.{ BufferedWriter, File, FileWriter, IOException },
    text.SimpleDateFormat,
    jutil.Calendar

import
  akka.actor.{ Actor, ActorRef, PoisonPill, ReceiveTimeout }

import
  play.api.Logger

class LogActor(id: Long, closeFunc: Long => Unit) extends Actor {

  import LogActorMessages._

  context.setReceiveTimeout(5 minutes)

  private val logFile = generateFile(id)

  override def receive = {
    case ReceiveTimeout =>
      replyCloseConnection(sender)
      markLogTimedOut()
      Logger.info(s"Actor for log with ID $id closed due to timeout")
      self ! PoisonPill
    case s: String =>
      handleMessage(s, sender)
    case msg =>
      sender ! Confused(msg.toString)
  }

  private def handleMessage(message: String, sender: ActorRef): Unit = {
    import LogActor.{ MessageSplitter => Message }
    message match {
      case Message("pulse", _) =>
        sender ! KeepAlive
      case Message("write", data) =>
        appendToFile(data, logFile)
        sender ! KeepAlive
      case Message("finalize", _) =>
        replyCloseConnection(sender)
        finalizeLog()
        Logger.info(s"Actor for log with ID $id reports mission accomplished")
        self ! PoisonPill
      case Message("abandon", _) =>
        replyCloseConnection(sender)
        logFile.delete()
        Logger.info(s"Actor for log with ID $id is aborting and deleting log")
        self ! PoisonPill
      case msg =>
        sender ! Confused(msg)
    }
  }

  private def replyCloseConnection(sender: ActorRef): Unit = {
    closeFunc(id)
    sender ! CloseConnection
  }

  private def generateFile(id: Long): File = {
    val timeFormat = new SimpleDateFormat("MM-dd-yy__HH'h'mm'm'ss's'")
    val filename = s"${timeFormat.format(Calendar.getInstance.getTime)}__sid${id}${LogActor.LogFileExtension}"
    createFile(filename)
  }

  private def createFile(name: String): File = {
    val file = new File(LogActor.ExpectedLogDir + File.separator + name)
    file.getParentFile.mkdirs()
    file.createNewFile()
    file
  }

  private def appendToFile(data: String, file: File): Unit = {
    Exception.ignoring(classOf[IOException]) {
      val writer = new FileWriter(file, true)
      val out = new BufferedWriter(writer)
      out.write(data.replaceAllLiterally("\r\n", "\n"))
      out.close()
      writer.close()
    }
  }

  // May not function properly with empty/improperly-formed files
  private def markLogTimedOut(): Unit = {
    val terminator = "" // if (!logProperlyTerminated(logFile)) LogActor.LogTerminator else ""
    val timeOutData = terminator + "\n<!-- Log terminated due to connection with WebStart client timing out -->"
    appendToFile(timeOutData + "\n", logFile)
  }

  // Right now, we do nothing to finalize logs
  private def finalizeLog(): Unit = {
    val finalData = ""
    appendToFile(finalData + "\n", logFile)
  }

  private def logProperlyTerminated(file: File): Boolean = {
    // readLastLineOfText(file).trim() == LogActor.LogTerminator
    true
  }

  private def readLastLineOfText(file: File): String = {
    val src = Source.fromFile(file)
    val last = src.getLines().foldLeft("") { case (acc, line) => if (line.matches(""".*[\w].*""")) line else acc }
    src.close()
    last
  }

}

object LogActor {
  val ExpectedLogDir = "nl_logs"
  val LogFileExtension = ".txt"
  // val LogTerminator = "</eventSet>"
  private val MessageSplitter = """(?s)([\w]+?)(?:\|(.*))?""".r // Messages are expected to be a [message type] followed by an optional ['|' and [data]]
}

object LogActorMessages {
  case object CloseConnection
  case class  Confused(msg: String)
  case object KeepAlive
}
