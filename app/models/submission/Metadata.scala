package models.submission

import
  scala.util.Try

import
  play.api.libs.json.{ JsObject, Json }

import
  scalaz.Scalaz.ToValidationV

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/24/12
 * Time: 1:30 PM
 */

trait Metadata {
  def getType: String
  def toMap:   Map[String, String]
}

class JsonMetadata(str: String) extends Metadata {
  override def getType = (Json.parse(str) \ "type").as[String]
  override def toMap   = Try(
    Json.parse(str).asInstanceOf[JsObject].value.mapValues(_.toString).asInstanceOf[Map[String, String]]
  ) getOrElse (
    Map()
  )
}

object Metadata extends FromStringParser {
  override protected type Target      = Metadata
  override protected type ConsTuple   = Nothing
  override protected type ParsedTuple = Nothing
  override def constructFrom(parsed: Parsed)   = ??? // It just doesn't seem worth it in this trivial case to fully implement things
  override def fromString(str: String): Output = {

    val metadata = new JsonMetadata(str)

    try {
      metadata.getType
      metadata.successNel
    }
    catch {
      case ex: Exception => ex.getMessage.failNel
    }

  }
}


