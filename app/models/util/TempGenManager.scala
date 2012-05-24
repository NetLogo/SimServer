package models.util

import java.net.URI
import java.io.File

import akka.actor.{ActorSystem, Actor}
import akka.util.duration._

import models.Delete

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempGenManager extends ActorSystem {

  private val TempGenPath  = "public/gen"
  private val CharEncoding = "UTF-8"
  private val LifeSpan     = 1 minute

  def registerFile(contents: String, fileName: String) : URI = {

    val filepath = new File("%s/%s".format(TempGenPath, java.net.URLEncoder.encode(fileName, CharEncoding))) getAbsolutePath()
    FileUtil.printToFile(filepath)(contents)

    // Create an actor with a handle to the file
    // The temp gen file is accessible for <LifeSpan> before being deleted
    val fileURI   = new URI(filepath)
    val fileActor = new TempGenActor(fileURI)
    scheduler.scheduleOnce(LifeSpan) {
      fileActor ! Delete
    }

    fileURI

  }

  // Could _easily_ be more efficient (at least for small numbers of files), but I want to stick to having actors manage the files
  def removeAll() {
    (new File(TempGenPath)).listFiles map (file => new TempGenActor(new URI(file.getAbsolutePath))) foreach (_ ! Delete)
  }

}

class TempGenActor(fileURI: URI) extends Actor {
  override protected def receive = {
    case Delete => new File(fileURI) delete()
  }
}
