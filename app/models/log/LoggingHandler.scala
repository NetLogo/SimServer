package models.log

import
  scala.{ collection, concurrent, util => util_scala },
    collection.mutable.ArrayBuffer,
    concurrent.{ Await, duration },
      duration._,
    util_scala.{ Random, Try }

import
  java.{ io, net, util => util_java },
    io.{ ByteArrayInputStream, File },
    net.URLDecoder,
    util_java.zip.GZIPInputStream

import
  akka.{ actor, pattern, util => util_akka },
    actor.{ ActorSystem, PoisonPill, Props },
    pattern.ask,
    util_akka.Timeout

import
  play.api.Logger

import play.api.Play.current

object LoggingHandler {

  val DefaultEncoding = "ISO-8859-1"

  private lazy val system = ActorSystem("Logging")

  def createNewLog(): Long = {
    ensureLogDirExists()
    val id = Random.nextInt().abs
    system.actorOf(Props(new LogActor(id, closeLog)), name = id.toString)
    id
  }

  def log(key: Long, data: String): String = {
    implicit val timeout = Timeout(5 seconds)
    val actor = system.actorSelection(s"/user/$key")
    Try(Await.result(actor ? prepareData(data), timeout.duration).toString + '\n') getOrElse "File already closed"
  }

  // If there's ever a desire for teachers to access logs, this could be useful.  Until then... it probably shouldn't exist.
//  def retrieveLogText(key: Long): String = {
//    val logDir = new File(LogActor.ExpectedLogDir)
//    val arr = logDir.listFiles(new FilenameFilter(): Unit = {
//      def accept(file: File, name: String): Boolean = {
//        name.toLowerCase.endsWith(s"sid$key${LogActor.LogFileExtension}")
//      }
//    })
//    arr.lastOption map {  x => val src = Source.fromFile(x); val lines = src.getLines().mkString("\n"); src.close(); lines } getOrElse
//    ("Invalid log key given: " + key)
//  }

  private[models] def closeLog(id: Long): Unit =
    system.actorSelection(s"/user/$id") ! PoisonPill

  private def ensureLogDirExists(): Unit = {
    val logDir = new File(LogActor.ExpectedLogDir)
    if (!logDir.exists()) logDir.mkdir()
  }

  private def prepareData(data: String, encoding: String = DefaultEncoding): String =
    (
      unGzip(data.getBytes(encoding)) flatMap validateData
    ) orElse (
      decompressData(data, encoding) orElse Option(data) flatMap validateData
    ) getOrElse (
      "ERROR   DATA MUST BE TEXT OR GZIP COMPRESSED" replaceAll(" ", "_")
    )

  /* Some(data)  if valid
   * None        if invalid
   */
  private def validateData(data: String): Option[String] = {
    if (isValid(data)) Option(data) else None
  }

  private def decompressData(data: String, encoding: String = DefaultEncoding): Option[String] = {
    unGzip(URLDecoder.decode(data, encoding).getBytes(encoding))
  }

  private def unGzip(data: Array[Byte]): Option[String] =
    try {

      val in = new GZIPInputStream(new ByteArrayInputStream(data))
      val buffer = new ArrayBuffer[Byte]

      while (in.available() > 0)
        buffer.append(in.read().toByte)

      in.close()
      Some(buffer.map(_.toChar).mkString.reverse dropWhile (_.toByte < 0) reverse) // Make string and trim off any nonsense at end

    }
    catch {
      case ex: Exception =>
        Logger.warn("Failed to un-GZIP log data",  ex)
        None
    }

  // I used to sass this validation, but, honestly... it's not the worst
  private def isValid(data: String): Boolean = data.toLowerCase.matches("""(?s)^[a-z]{3}.*""")

}
