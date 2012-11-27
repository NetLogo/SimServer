package models.filemanager

import java.io.File

import akka.actor._
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.{ duration, Duration, Timeout }, duration._

import models.{ Get, Delete, Initialize, Write }
import models.util.FileUtil
import models.Write

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 8/22/12
 * Time: 1:02 PM
 */

trait FileManager extends Delayer {

  val PublicPath   = "public"
  val AssetPath    = "assets"
  def MyFolderName: String

  protected val CharEncoding = "UTF-8"
  protected def LifeSpan  : Duration
  protected def SystemName: String

  protected lazy val system      = ActorSystem(SystemName)
  protected lazy val fileFolder  = new File(PublicPath + File.separator + MyFolderName)

  def formatFilePath(fileNameBasis: String, fileExt: String) : String = {
    "%s/%s.%s".format(MyFolderName, fileNameBasis, fileExt)
  }

  def registerFile(contents: String, fileNameBasis: String, fileExt: String = "") : String = {
    val filename  = if (!fileExt.isEmpty) formatFilePath(fileNameBasis, fileExt) else fileNameBasis
    saveFile(contents, filename, fileNameBasis)
  }

  protected def saveFile(contents: String, filename: String, actorID: String) : String = {

    val file      = new File("%s%s%s".format(PublicPath, File.separator, filename))
    val fileActor = {
      val actorName = idToActorName(actorID)
      try system.actorOf(Props(new FileActor(file)), name = actorName)
      catch {
        case ex: InvalidActorNameException => system.actorFor(actorName)
      }
    }

    fileActor ! Initialize
    fileActor ! Write(contents)

    // The temp gen file is accessible for <LifeSpan> before being deleted
    system.scheduler.scheduleOnce(LifeSpan) { fileActor ! Delete }
    file.toString.replace(PublicPath, AssetPath)

  }

  def retrieveFile(fileNameBasis: String) : File = {
    implicit val timeout = Timeout(3 seconds)
    Await.result(system.actorFor(idToActorName(fileNameBasis)) ? Get, timeout.duration).asInstanceOf[File]
  }

  // Could _easily_ be more efficient (at least for small numbers of files), but I want to stick to having actors manage the files
  def removeAll() {
    fileFolder.listFiles foreach { file => system.actorOf(Props(new FileActor(file))) ! Delete }
  }

  protected def idToActorName(id: String) = id.replaceAll("/", "&~&")

}

// v--  DEFINITIONS BELOW ARE OPEN TO EXTRACTION/REFACTORING  --v

class FileActor(file: File) extends Actor {
  override protected def receive = {
    case Get             => file
    case Delete          => file.delete(); self ! PoisonPill // Terminate self after file is gone
    case Initialize      => file.getParentFile.mkdirs(); file.delete(); file.createNewFile()
    case Write(contents) => FileUtil.printBytesToFile(file.getAbsolutePath)(contents)
  }
}

// This was created to seamlessly hide nitty-gritty detail that a class's body is delayed init (usually for superfluous reasons)
// Why does this not already exist in the Scala library to begin with? --JAB (8/29/12)
sealed trait Delayer extends DelayedInit {
  override def delayedInit(body: => Unit) {
    body
  }
}
