package models.submission

import play.api.libs.json._

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 11/9/12
 * Time: 2:23 PM
 */

trait JsonWritable {
  def toJsonObj : JsObject
  def toJson = toJsonObj.toString
}

object ToJsonConverters {

  implicit def work2JsonWritable(work: UserWork) : JsonWritable = new JsonWritable {
    import work._
    override def toJsonObj : JsObject = {
      val periodIDTuple    = ("period_id",   JsString(periodID))
      val runIDTuple       = ("run_id",      JsString(runID))
      val userIDTuple      = ("user_id",     JsString(userID))
      val typeTuple        = ("type",        JsString(typ))
      val dataTuple        = ("data",        JsString(data))
      val metadataTuple    = ("metadata",    Json.parse(metadata))
      val descriptionTuple = ("description", JsString(description))
      val supplementsTuple = ("supplements", Json.toJson(supplements map (_.toJsonObj)))
      val commentsTuple    = ("comments",    Json.toJson(comments map (_.toJsonObj)))
      val tuples         = Seq(periodIDTuple, runIDTuple, userIDTuple, typeTuple, dataTuple,
        metadataTuple, descriptionTuple, supplementsTuple, commentsTuple)
      JsObject(tuples)
    }
  }

  implicit def comment2JsonWritable(comment: UserWorkComment) : JsonWritable = new JsonWritable {
    override def toJsonObj : JsObject = {
      val timestampTuple = ("timestamp", JsNumber(comment.timestamp))
      val userIDTuple    = ("user_id",   JsString(comment.userID))
      val commentTuple   = ("comment",   JsString(comment.comment))
      val tuples         = Seq(timestampTuple, userIDTuple, commentTuple)
      JsObject(tuples)
    }
  }

  implicit def supplement2JsonWritable(supplement: UserWorkSupplement) : JsonWritable = new JsonWritable {
    import supplement._
    override def toJsonObj : JsObject = {
      val typeTuple     = ("type",     JsString(typ))
      val dataTuple     = ("data",     JsString(data))
      val metadataTuple = ("metadata", Json.parse(metadata))
      val tuples         = Seq(typeTuple, dataTuple, metadataTuple)
      JsObject(tuples)
    }
  }

}
