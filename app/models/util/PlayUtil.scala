package models.util

import
  play.api.{ libs, Logger, mvc },
    libs.json._,
    mvc.{ AnyContent, Request }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/5/12
 * Time: 5:03 PM
 */

object PlayUtil {

  def commonExtractMap(request: Request[AnyContent]) : Map[String, String] =
    extractBundle(request).stringParams

  // If Play actually made a good-faith effort at parameter extraction, I wouldn't have to go through this rubbish...
  def extractBundle(request: Request[AnyContent]) : ParamBundle =
    request.body.asMultipartFormData map {
      formData =>
        val fileKVs = formData.files map {
          formFile =>
            import scala.io.{ Codec, Source }
            val file = formFile.ref.file
            val arr  = if (file.length < 20000000L) Source.fromFile(file)(Codec.ISO8859).map(_.toByte).toArray else "UPLOADED FILE TOO LARGE".getBytes
            (formFile.key, arr)
        }
        ParamBundle(formData.asFormUrlEncoded, fileKVs.toMap)
    } orElse {
      request.body.asFormUrlEncoded flatMap (Util.noneIfEmpty(_)) map (ParamBundle(_))
    } orElse {
      Option(request.queryString) map (ParamBundle(_))
    } getOrElse {
      ParamBundle(Map(), Map())
    }

  // Try _really_ hard to parse the body into JSON (pretty much the only thing I don't try is XML conversion)
  def extractJSONOpt(request: Request[AnyContent]) : Option[JsValue] = {
    val body = request.body
    body.asJson orElse {
      try {
        body.asText orElse {
          body.asRaw flatMap (_.asBytes() map (new String(_)))
        } map (Json.parse(_))
      }
      catch {
        case ex: Exception =>
          Logger.info("Failed to parse text into JSON", ex)
          None
      }
    } orElse {
      (extractBundle _ andThen (_.stringSeqParams) andThen paramMap2JSON _)(request)
    }
  }

  private def stringSeq2JSONOpt(seq: Seq[String]) : Option[JsValue] = {

    def generousParse(str: String) : JsValue = {
      try Json.parse(str)
      catch {
        case ex: Exception => JsString(str) // Ehh...
      }
    }

    try {
      seq.toList match {
        case Nil      => None
        case h :: Nil => Option(generousParse(h))
        case arr      => Option(new JsArray(arr map (generousParse(_))))
      }
    }
    catch {
      case ex: Exception =>
        Logger.info("Failed to parse string sequence into JSON", ex)
        None
    }

  }

  private def paramMap2JSON(paramMap: Map[String, Seq[String]]) : Option[JsValue] = {
    val parsedParams    = paramMap map { case (k, v) => (k, stringSeq2JSONOpt(v)) }
    val validatedParams = parsedParams collect { case (k, Some(v)) => (k, v) }
    if (!validatedParams.isEmpty) Option(new JsObject(validatedParams.toSeq)) else None
  }

}


case class ParamBundle(stringSeqParams: Map[String, Seq[String]], byteParams: Map[String, Array[Byte]] = Map()) {
  lazy val stringParams = stringSeqParams mapValues (_.head)
}
