package models

import java.util.zip.GZIPInputStream
import collection.mutable.{ArrayBuffer, HashMap}
import io.Source
import util.Random
import actors.Actor.State._
import java.io.{FilenameFilter, ByteArrayInputStream, File}

object LoggingHandler {

  //@ Will need a better ID-distro system than this, because we need this to persist and not overread people's stuff!
  private var logCount = 0L

  private val idActorMap = new HashMap[Long, LogActor]()

  def createNewLog() : Long = {
    ensureLogDirExists()
    //@ val id = { logCount += 1; logCount }
    val id = Random.nextInt().abs.toInt
    val actor = new LogActor(id)
    idActorMap.put(id, actor)
    actor.start()
    id
  }

  def log(key: Long, data: String) : String = {
    idActorMap.get(key) map { case x if (x.getState != Terminated) => (x !? prepareData(data)).asInstanceOf[String] + '\n' } getOrElse ("File already closed")
  }

  //@ This really needs to be wrapped behind a login-/password-check (as does abandoning)
  def retrieveLogText(key: Long) : String = {
    val logDir = new File(LogActor.ExpectedLogDir)
    val arr = logDir.listFiles(new FilenameFilter() {
      def accept(file: File, name: String) : Boolean = {
        name.toLowerCase.endsWith("sid%d.xml".format(key))
      }
    })
    arr.lastOption map { x => val src = Source.fromFile(x); val lines = src.getLines().mkString("\n"); src.close(); lines } getOrElse
            ("Invalid log key given: " + key)
  }

  private[models] def closeLog(id: Long) {
    idActorMap.remove(id)
  }

  private def ensureLogDirExists() {
    val logDir = new File(LogActor.ExpectedLogDir)
    if (!logDir.exists()) logDir.mkdir()
  }

  private def prepareData(data: String) : String = {
    decompressData(data) orElse Option(data) flatMap (validateData(_)) getOrElse ("ERROR   DATA MUST BE TEXT OR GZIP COMPRESSED" replaceAll(" ", "_"))
  }

  /* Some(data)  if valid
   * None        if invalid
   */
  private def validateData(data: String) : Option[String] = {
    if (data.length() > 2 && data.toLowerCase.matches("""(?s)^[a-z][a-z][a-z].*""")) Some(data) else None  //@ Pretty unmaginificent validation on my part...
  }

  private def decompressData(data: String) : Option[String] = {
    try {

      val in = new GZIPInputStream(new ByteArrayInputStream(data.getBytes))  // Don't add an encoding to `getBytes`; it's already encoded as GZIP format
      val buffer = new ArrayBuffer[Byte]

      while (in.available() > 0)
        buffer.append(in.read().toByte)

      in.close()
      Some(buffer.map(_.toChar).mkString)//! dropRight 1 // Drops the '?' that gets added to the end of the string

    }
    catch {
      case e => None
    }
  }
  
}
