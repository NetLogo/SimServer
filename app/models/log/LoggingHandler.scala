package models.log

import java.util.zip.GZIPInputStream
import collection.mutable.{ArrayBuffer, HashMap}
import io.Source
import scala.util.Random
import actors.Actor.State._
import java.io.{FilenameFilter, ByteArrayInputStream, File}

object LoggingHandler {

  //@ Will need a better ID-distro system than this, because we need this to persist and not overread people's stuff!
  private var logCount = 0L

  val DefaultEncoding = "ISO-8859-1"
  private val idActorMap = new HashMap[Long, LogActor]()

  def createNewLog(): Long = {
    ensureLogDirExists()
    //@ val id = { logCount += 1; logCount }
    val id = Random.nextInt().abs.toInt
    val actor = new LogActor(id)
    idActorMap.put(id, actor)
    actor.start()
    id
  }

  def log(key: Long, data: String): String = {
    idActorMap.get(key) map { case x if (x.getState != Terminated) => (x !? prepareData(data)).asInstanceOf[String] + '\n' } getOrElse ("File already closed")
  }

  //@ This really needs to be wrapped behind a login-/password-check (as does abandoning)
  def retrieveLogText(key: Long): String = {
    val logDir = new File(LogActor.ExpectedLogDir)
    val arr = logDir.listFiles(new FilenameFilter() {
      def accept(file: File, name: String): Boolean = {
        name.toLowerCase.endsWith("sid%d%s".format(key, LogActor.LogFileExtension))
      }
    })
    arr.lastOption map {  x => val src = Source.fromFile(x); val lines = src.getLines().mkString("\n"); src.close(); lines } getOrElse
    ("Invalid log key given: " + key)
  }

  private[models] def closeLog(id: Long) {
    idActorMap.remove(id)
  }

  private def ensureLogDirExists() {
    val logDir = new File(LogActor.ExpectedLogDir)
    if (!logDir.exists()) logDir.mkdir()
  }

  private def prepareData(data: String): String = {
    decompressData(data) orElse Option(data) flatMap (validateData(_)) getOrElse
    ("ERROR   DATA MUST BE TEXT OR GZIP COMPRESSED" replaceAll(" ", "_"))
  }

  /* Some(data)  if valid
   * None        if invalid
   */
  private def validateData(data: String): Option[String] = {
    if (isValid(data)) Option(data) else None
  }

  private def decompressData(data: String, encoding: String = DefaultEncoding): Option[String] = {
    unGzip(java.net.URLDecoder.decode(data, encoding).getBytes(encoding))
  }

  private def unGzip(data: Array[Byte]): Option[String] = {
    try {

      val in = new GZIPInputStream(new ByteArrayInputStream(data))
      val buffer = new ArrayBuffer[Byte]

      while (in.available() > 0)
        buffer.append(in.read().toByte)

      in.close()
      Some(buffer.map(_.toChar).mkString.reverse dropWhile (_.toByte < 0) reverse) // Make string and trim off any nonsense at end

    }
    catch {
      case _ => None
    }
  }

  def isHandlable(data: String): Boolean = {
    isGzipDecodable(data) || isValid(data)
  }

  private def isValid(data: String): Boolean = {
    data.toLowerCase.matches("""(?s)^[a-z]{%d}.*""".format(3)) //@ Pretty unmaginificent validation on my part...
  }

  private def isGzipDecodable(data: String, encoding: String = DefaultEncoding): Boolean = {
    /*
     //@
     This `try`/`catch` thing actually shouldn't be necessary.  Somehow, "finalize" messages—and _only_ "finalize"
     messages—are reaching this point and not being able to be decoded by `URLDecoder.decode`.  It complains of
     invalid hex characters.  However, the "last-ditch effort" code that calls `RequestUtil.extractPropertyFromUri`
     to read the compressed text out of the URI succeeds, so... it's unclear how this could be going wrong.
     Fortunately, the code _works_ as is, and I'm not sure that this is big deal.  If someone ends up reading this,
     though, that probably means that it ended up being a big deal....  --JAB
     */
    val decodedData = {
      try {
        java.net.URLDecoder.decode(data, encoding).getBytes(encoding)
      }
      catch {
        case _ => data.getBytes(encoding)
      }
    }
    try {
      new GZIPInputStream(new ByteArrayInputStream(decodedData))
      true
    }
    catch {
      case _ => false
    }
  }

}
