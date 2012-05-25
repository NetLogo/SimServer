package models.util

import java.net.URI
import java.io.File

import akka.util.duration._
import akka.actor.{PoisonPill, ActorSystem, Actor}

import models.{Write, Delete}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempGenManager {

  val PublicPath = "public"
  val TempGenPath  = "gen"
  private val CharEncoding = "UTF-8"
  private val LifeSpan     = 1 minute

  private val system = ActorSystem("TempGen")

  def formatFilePath(fileName: String) : String = "%s/%s".format(TempGenPath, java.net.URLEncoder.encode(fileName, CharEncoding))

  def registerFile(contents: String, fileName: String) : URI = {

    // Create an actor with a handle to the file, write the contents to it
    val fileURI = new URI(new File("%s/%s".format(PublicPath, formatFilePath(fileName))) getAbsolutePath())
    val fileActor = new TempGenActor(fileURI)
    fileActor.self ! Write(contents)

    // The temp gen file is accessible for <LifeSpan> before being deleted
    system.scheduler.scheduleOnce(LifeSpan) { fileActor.self ! Delete }

    fileURI

  }

  // Could _easily_ be more efficient (at least for small numbers of files), but I want to stick to having actors manage the files
  def removeAll() {
    //!(new File(TempGenPath)).listFiles map (file => new TempGenActor(new URI(file.getAbsolutePath))) foreach (_.self ! Delete)
  }

}

class TempGenActor(fileURI: URI) extends Actor {
  override protected def receive = {
    case Write(contents) => FileUtil.printToFile(fileURI.toString)(contents)
    case Delete          => new File(fileURI) delete(); self ! PoisonPill // Terminate self after file is gone
  }
}
