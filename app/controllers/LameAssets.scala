package controllers

import
  java.{ io, net, util },
    io.File,
    net.URL,
    util.{ Date, Locale }

import
  scala.util.Try

import
  org.joda.time.{ format, DateTimeZone },
    format.DateTimeFormat

import
  play.{ api, utils },
    api.{ libs, mvc, Play },
      libs.{ iteratee, MimeTypes },
        iteratee.Enumerator,
      mvc.{ Action, Controller, Result, ResponseHeader, SimpleResult },
    utils.UriEncoding

import Play.current

// Much of this code was appropriated from Play 2.2.3's `controller.Assets` file, but foregoing a lot of the nice
// caching stuff they have go on and allowing files to be newly-read from the disk in "Production" mode --JAB (5/11/14)
object LameAssets extends Controller {

  private val dateFormatter = {
    val timeZoneCode = "GMT"
    DateTimeFormat forPattern s"EEE, dd MMM yyyy HH:mm:ss '$timeZoneCode'" withLocale Locale.ENGLISH withZone DateTimeZone.forID(timeZoneCode)
  }

  def at(filepath: String) = Action {
    val urlFromResourceCache = resourceNameOptAt("public", filepath) flatMap Play.resource
    lazy val urlFromDisk     = fileOptAt("public", filepath) map (_.toURI.toURL)
    val urlOpt               = urlFromResourceCache orElse urlFromDisk
    val resultOpt            = urlOpt flatMap buildResultOpt(filepath)
    resultOpt getOrElse NotFound
  }

  private def buildResultOpt(filepath: String)(url: URL): Option[Result] = {

    val lengthAndContentsOpt = {
      import play.api.libs.concurrent.Execution.Implicits.defaultContext
      val stream = url.openStream()
      Try((stream.available, Enumerator.fromStream(stream))).toOption
    }

    lengthAndContentsOpt map {
      case (length, contents) =>

        val headerParams = Map(
          CONTENT_LENGTH -> length.toString,
          CONTENT_TYPE   -> MimeTypes.forFileName(filepath).map(withCharset).getOrElse(BINARY),
          DATE           -> dateFormatter.print(new Date().getTime())
        )

        val header = ResponseHeader(OK, headerParams)

        SimpleResult(header, contents)

    }

  }

  private def resourceNameOptAt(path: String, file: String): Option[String] = {

    val properPath   = getProperFilePath(path, file)
    val resourceName = Option(properPath).map(name => if (name startsWith "/") name else s"/$name").get
    val resourceFile = new File(properPath)

    if (resourceFile.exists() && !resourceFile.isDirectory())
      Some(resourceName)
    else
      None

  }

  private def fileOptAt(path: String, file: String): Option[File] = {

    val resourceFile = new File(getProperFilePath(path, file))

    if (resourceFile.exists() && !resourceFile.isDirectory())
      Some(resourceFile)
    else
      None

  }

  private def getProperFilePath(prefix: String, path: String): String = {
    val decodedPath = UriEncoding.decodePath(path, "utf-8")
    s"$prefix/$decodedPath"
  }

  private def withCharset(mimeType: String): String =
    if (MimeTypes.isText(mimeType))
      s"$mimeType; charset=${Play.configuration.getString("default.charset").getOrElse("utf-8")}"
    else
      mimeType

}
