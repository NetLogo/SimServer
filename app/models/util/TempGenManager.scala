package models.util

import java.io.File

import akka.util.duration._
import akka.actor.{PoisonPill, ActorSystem, Actor, Props}
import models.{Initialize, Write, Delete}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempGenManager {

  val PublicPath = "public"
  val AssetPath  = "assets"
  val TempGenPath  = "gen"
  private val CharEncoding = "UTF-8"
  private val LifeSpan     = 1 minute

  private val system = ActorSystem("TempGen")
  private val tempGenFolder = new File(PublicPath + File.separator + TempGenPath)

  if (!tempGenFolder.exists()) tempGenFolder.mkdir()

  def formatFilePath(fileName: String, fileExt: String) : String = {
    val (dropNum, takeNum) = (10, 16)      // The first 10 characters tend to be encryption noise; 16 characters should give good uniqueness
    val encoded = java.net.URLEncoder.encode(fileName, CharEncoding) filterNot (_ == '%')        // Play won't route to a file with a '%' in
    val name = if (encoded.length >= dropNum + 5) encoded drop dropNum take takeNum else encoded // If it meets my magic buffer size, trim it
    "%s/%s.%s".format(TempGenPath, name, fileExt)
  }

  def registerFile(contents: String, rawFileName: String, fileExt: String) : String = {
    registerFile(contents, formatFilePath(rawFileName, fileExt))
  }

  def registerFile(contents: String, fileName: String) : String = {

    // Create an actor with a handle to the file, write the contents to it
    val file = new File("%s%s%s".format(PublicPath, File.separator, fileName))
    val fileActor = system.actorOf(Props(new TempGenActor(file)))
    fileActor ! Initialize
    fileActor ! Write(contents)

    // The temp gen file is accessible for <LifeSpan> before being deleted
    system.scheduler.scheduleOnce(LifeSpan) { fileActor ! Delete }

    file.toString.replace(PublicPath, AssetPath)

  }

  // Could _easily_ be more efficient (at least for small numbers of files), but I want to stick to having actors manage the files
  def removeAll() {
    tempGenFolder.listFiles foreach { file => system.actorOf(Props(new TempGenActor(file))) ! Delete }
  }

}

class TempGenActor(file: File) extends Actor {
  override protected def receive = {
    case Initialize      => file.delete(); file.createNewFile()
    case Write(contents) => FileUtil.printToFile(file)(_.write(contents))
    case Delete          => file.delete(); self ! PoisonPill // Terminate self after file is gone
  }
}
