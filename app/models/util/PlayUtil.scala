package models.util

import play.api.{ Logger, mvc }
import play.api.libs.json._
import mvc.{ AnyContent, Request }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/5/12
 * Time: 5:03 PM
 */

object PlayUtil {

  def commonExtractMap(request: Request[AnyContent]) : Map[String, String] =
    extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }

  // If Play actually made a good-faith effort at parameter extraction, I wouldn't have to go through this rubbish...
  def extractParamMapOpt(request: Request[AnyContent]) : Option[Map[String, Seq[String]]] =
    request.body.asMultipartFormData.map(_.asFormUrlEncoded).
            orElse(request.body.asFormUrlEncoded flatMap (Util.noneIfEmpty(_))).
            orElse(Option(request.queryString))

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
    } orElse { extractParamMapOpt(request) flatMap paramMap2JSON }
  }

  private def stringSeq2JSONOpt(seq: Seq[String]) : Option[JsValue] = {

    def generousParse(str: String) : JsValue = {
      try Json.parse(str)
      catch {
        case ex: com.codahale.jerkson.ParsingException => JsString(str)
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
