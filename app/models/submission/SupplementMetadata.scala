package models.submission

import play.api.libs.json.Json

import scalaz.Success

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/24/12
 * Time: 1:30 PM
 */

trait SupplementMetadata {
  def getType: String
}

object SupplementMetadata extends FromStringParser {
  override protected type Target    = SupplementMetadata
  override protected type ConsTuple = Nothing
  override def fromString(str: String) : Output = {
    Success(new SupplementMetadata { override def getType = (Json.parse(str) \ "type").as[String] })
  }
}


