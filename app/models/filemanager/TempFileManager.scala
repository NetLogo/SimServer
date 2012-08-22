package models.filemanager

import java.io.File

import akka.util.duration._
import akka.actor.{PoisonPill, ActorSystem, Actor, Props}

import models.{Initialize, Write, Delete}
import models.util.FileUtil

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempFileManager extends FileManager {

  override val MyFolderName = "gen"

  protected override val LifeSpan = 1 minute

  private val system = ActorSystem("TempGen")
  private val tempGenFolder = new File(PublicPath + File.separator + MyFolderName)

  if (!tempGenFolder.exists()) tempGenFolder.mkdir()

  def formatFilePath(fileNameBasis: String, fileExt: String) : String = {
    "%s/%s.%s".format(MyFolderName, fileNameBasis.##.abs, fileExt)
  }

  def registerFile(contents: String, fileName: String): String = {

    // Create an actor with a handle to the file, write the contents to it
    val file = new File("%s%s%s".format(PublicPath, File.separator, fileName))
    val fileActor = system.actorOf(Props(new TempFileActor(file)))
    fileActor ! Initialize
    fileActor ! Write(contents)

    // The temp gen file is accessible for <LifeSpan> before being deleted
    system.scheduler.scheduleOnce(LifeSpan) { fileActor ! Delete }
    file.toString.replace(PublicPath, AssetPath)

  }

  // Could _easily_ be more efficient (at least for small numbers of files), but I want to stick to having actors manage the files
  def removeAll() {
    tempGenFolder.listFiles foreach { file => system.actorOf(Props(new TempFileActor(file))) ! Delete }
  }

}

class TempFileActor(file: File) extends Actor {
  override protected def receive = {
    case Initialize      => file.delete(); file.createNewFile()
    case Write(contents) => FileUtil.printToFile(file)(_.write(contents))
    case Delete          => file.delete(); self ! PoisonPill // Terminate self after file is gone
  }
}
