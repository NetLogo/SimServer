package models.submission

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 2:00 PM
 */

sealed trait Submission {
  def id: Option[Long]
}

trait Association extends Submission {
  def refID : Option[Long]
}

trait Entry extends Submission

trait JsonWritable {
  def toJsonObj : play.api.libs.json.JsObject
  def toJson = toJsonObj.toString
}