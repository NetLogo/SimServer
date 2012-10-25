package models.parse.submission

import models.submission.SupplementMetadata

import play.api.libs.json.Json

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/25/12
 * Time: 4:40 PM
 */

object SupplementMetadataParser {
  def apply(str: String) : SupplementMetadata = {
    new SupplementMetadata { override def getType = (Json.parse(str) \ "type").as[String] }
  }
}
