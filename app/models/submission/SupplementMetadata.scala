package models.submission

import play.api.libs.json.Json

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/24/12
 * Time: 1:30 PM
 */

//@ Not sure how I want to handle this right now...
trait SupplementMetadata {
  def getType: String
}

object SupplementMetadataParser {
  def apply(str: String) : SupplementMetadata = {
    new SupplementMetadata { override def getType = (Json.parse(str) \ "type").as[String] }
  }
}