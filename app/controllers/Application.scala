package controllers

import
  java.{ io, nio },
    io.{ File, FilenameFilter, PrintWriter },
    nio.file.{ attribute, Files },
      attribute.BasicFileAttributes

import
  scala.concurrent.duration._

import
  play.{ api, libs },
    api.mvc.{ Action, Controller },
    libs.Akka

import
  controllers.action.APIAction

import
  models.util.{ PlayUtil, Util }

object Application extends Controller {

  /*
   "Wow!  WHAT IS THIS?!", you ask.  Well, let me tell you...
   First, look into the HTTP request type OPTIONS.  Then, understand that this simply replies to the OPTIONS request,
   saying that we'll accept pretty much any request--cross-domain, or not
   */
  def options(nonsense: String) = APIAction { Ok }

  def init(): Unit = {

    def findCreationTime(file: File): Long =
      Files.readAttributes(file.toPath, classOf[BasicFileAttributes]).creationTime().toMillis

    val fileFilter = new FilenameFilter {
      override def accept(dir: File, name: String) = name.matches("""sbt\d+\.log""")
    }

    val tempDir  = new File(System.getProperty("java.io.tmpdir"))
    val files    = tempDir.listFiles(fileFilter)
    val headFile = files.headOption getOrElse (throw new Exception("Somehow, there isn't an SBT log file present for us..."))

    // Grab the most recent log
    val logFile = files.tail.foldLeft((headFile, findCreationTime(headFile))) {
      case ((file, time), x) =>
        val xTime = findCreationTime(x)
        if (xTime > time)
          (x, xTime)
        else
          (file, time)
    }._1

    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    // Clear the file every ten minutes
    Akka.system.scheduler.schedule(0 seconds, 10 minutes) {
      val writer = new PrintWriter(logFile)
      writer.write("")
      writer.close()
    }

  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def redirect(path: String) = Action {
    MovedPermanently(s"/$path")
  }

  def displayHttpRequest = APIAction {
    request =>
      val bundle = PlayUtil.extractBundle(request)
      val text = s"""
                   |Request Type:
                   |${request.method}
                   |
                   |Headers:
                   |${request.headers.toSimpleMap map { case (k, v) => s"$k: $v" } mkString "\n"}
                   |
                   |Body:
                   |${Util.noneIfEmpty(bundle.stringParams, (_: Map[String, String]) map { case (k, v) => s"$k=$v" } mkString "\n") getOrElse "[empty]"}
                   |
                   |${bundle.byteParams map { case (k, v) => s"$k={{{${new String(v)}}}}" } mkString "\n"}""".stripMargin
      Ok(text)
  }

}

