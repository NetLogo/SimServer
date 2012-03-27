package models

import java.util.zip.GZIPInputStream
import collection.mutable.{ArrayBuffer, HashMap}
import io.Source
import util.Random
import java.io.{FilenameFilter, ByteArrayInputStream, File}
import actors.Actor.State._

object LoggingHandler {

  //@ Will need a better ID-distro system than this, because we need this to persist and not overread people's stuff!
  private var logCount = 0L

  private val idActorMap = new HashMap[Long, LogActor]()
  val ByteEncoding = "UTF-8"

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
    idActorMap.get(key) map { case x if (x.getState != Terminated) => (x !? decompressData(data)).asInstanceOf[String] } getOrElse("File already closed")
  }

  //@ This really needs to be wrapped behind a login-/password-check (as does abandoning)
  def retrieveLogText(key: Long) : String = {
    val logDir = new File(LogActor.ExpectedLogDir)
    val arr = logDir.listFiles(new FilenameFilter() {
      def accept(file: File, name: String) : Boolean = {
        name.toLowerCase.endsWith("sid%d.xml".format(key))
      }
    })
    val file = if (arr.isEmpty) None else Some(arr.last)
    file map { x => val src = Source.fromFile(x); val lines = src.getLines().mkString("\n"); src.close(); lines } getOrElse("Invalid log key given: " + key)
  }

  private[models] def closeLog(id: Long) {
    idActorMap.remove(id)
  }

  private def ensureLogDirExists() {
    val logDir = new File(LogActor.ExpectedLogDir)
    if (!logDir.exists()) logDir.mkdir()
  }

  private def decompressData(data: String) : String = {

    //@ val in = new GZIPInputStream(new ByteArrayInputStream(data.getBytes(ByteEncoding)))
    val in = new ByteArrayInputStream(data.getBytes(ByteEncoding))
    val buffer = new ArrayBuffer[Byte]

    while (in.available() > 0)
      buffer.append(in.read().toByte)

    in.close()
    buffer.map(_.toChar).mkString + '\n'
    
  }
  
}
