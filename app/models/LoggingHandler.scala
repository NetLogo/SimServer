package models

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream
import collection.mutable.{ArrayBuffer, HashMap}

//@ Pretty sloppy so far...
object LoggingHandler {

  //@ Will need a better ID-distro system than this, because we need this to persist and not overwrite people's stuff!
  private var logCount = 0L

  private val idActorMap = new HashMap[Long, LogActor]()
  val ByteEncoding = "UTF-8"

  def createNewLog() : Long = {
    ensureLogDirExists()
    val id = { logCount += 1; logCount }
    val actor = new LogActor(id)
    idActorMap.put(id, actor)
    actor.start()
    id
  }

  def log(key: Long, data: String) {
    //@ Should we do something here about continuing to push data after the log has been closed?
    idActorMap.get(key) foreach (x => if (x.getState != actors.Actor.State.Terminated) x ! decompressData(data))
  }

  private[models] def closeLog(id: Long) {
    idActorMap.remove(id)
  }

  private def ensureLogDirExists() {
    val logDir = new java.io.File(LogActor.ExpectedLogDir)
    if (!logDir.exists()) logDir.mkdir()
  }

  private def decompressData(data: String) : String = {

    val in = new GZIPInputStream(new ByteArrayInputStream(data.getBytes(ByteEncoding)))
    val buffer = new ArrayBuffer[Byte]

    while (in.available() > 0)
      buffer.append(in.read().toByte)

    in.close()
    buffer.toString()
    
  }
  
}
