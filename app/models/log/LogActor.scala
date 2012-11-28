package models.log

import actors.{ Actor, TIMEOUT }
import scala.util.control.Exception

import java.io.File

class LogActor(id: Long, closeFunc: Long => Unit) extends Actor {

  private val logFile = generateFile(id)

  def act() {
    loop {
      reactWithin(15000) {
        case TIMEOUT   => replyCloseConnection(); markLogTimedOut(); exit("Timed out")
        case s: String =>
          val (msgType, data) = LogActor.MessageSplitter.findFirstMatchIn(s) map (x => (x.group(1), x.group(2))) getOrElse ("unrecognized_type", "error_data")
          msgType match {
            case "pulse"    => replyOk()
            case "write"    => appendToFile(data, logFile); replyOk()
            case "finalize" => replyCloseConnection(); finalizeLog(); exit("Mission Accomplished")
            case "abandon"  => replyCloseConnection(); logFile.delete(); exit("Process abandoned; file deleted")
            case msg        => replyConfused(msg)
          }
        case msg => replyConfused(msg.toString)
      }
    }
  }

  // Replies are currently not used, but... if I decide to use them in some way, it's not to have them already in place.
  private def replyOk() {
    reply("Ok")
  }

  private def replyCloseConnection() {
    closeFunc(id)
    reply("Close connection")
  }

  private def replyConfused(msg: String) {
    reply("Unable to process message: " + msg)
  }

  private def generateFile(id: Long): File = {
    import java.text.SimpleDateFormat, java.util.Calendar
    val timeFormat = new SimpleDateFormat("MM-dd-yy__HH'h'mm'm'ss's'")
    val filename = "%s__sid%d%s".format(timeFormat.format(Calendar.getInstance().getTime), id, LogActor.LogFileExtension)
    createFile(filename)
  }

  private def createFile(name: String): File = {
    val file = new File(LogActor.ExpectedLogDir + File.separator + name)
    file.createNewFile()
    file
  }

  private def appendToFile(data: String, file: File) {
    import java.io.{BufferedWriter, FileWriter}
    Exception.ignoring(classOf[java.io.IOException]) {
      val writer = new FileWriter(file, true)
      val out = new BufferedWriter(writer)
      out.write(data.replaceAllLiterally("\r\n", "\n") + "\n")
      out.close()
      writer.close()
    }
  }

  // May not function properly with empty/improperly-formed files
  private def markLogTimedOut() {
    val terminator = "" // if (!logProperlyTerminated(logFile)) LogActor.LogTerminator else ""
    val timeOutData = terminator + "\n<!-- Log terminated due to connection with WebStart client timing out -->"
    appendToFile(timeOutData, logFile)
  }

  // Right now, we do nothing to finalize logs
  private def finalizeLog() {
    val finalData = ""
    appendToFile(finalData, logFile)
  }

  private def logProperlyTerminated(file: File): Boolean = {
    // readLastLineOfText(file).trim() == LogActor.LogTerminator
    true
  }

  private def readLastLineOfText(file: File): String = {
    import scala.io.Source
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
  private val MessageSplitter = """(?s)([\w]+)\|?(.*)""".r // Messages are expected to be a [message type] followed by an optional ['|' and [data]]
}
