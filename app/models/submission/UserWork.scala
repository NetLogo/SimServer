package models.submission

import play.api.libs.json._

import models.submission, submission.{ UserWorkComment => Comment }, submission.{ UserWorkSupplement => Supplement }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:14 PM
 */

case class UserWork(override val id:          Option[Long] = None,
                                 timestamp:   Long = System.currentTimeMillis(),
                                 periodID:    String,
                                 runID:       String,
                                 userID:      String,
                                 typ:         String,
                                 data:        String,
                                 metadata:    String,
                                 description: String,
                                 supplements: Seq[Supplement],
                                 comments:    Seq[Comment]) extends Entry with JsonWritable {

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

  def addComments   (newComments:    UserWorkComment*)    = this.cloneWith(comments = this.comments ++ newComments)
  def addSupplements(newSupplements: UserWorkSupplement*) = this.cloneWith(supplements = this.supplements ++ newSupplements)

  def cloneWith(id:          Option[Long]    = this.id,
                timestamp:   Long            = this.timestamp,
                periodID:    String          = this.periodID,
                runID:       String          = this.runID,
                userID:      String          = this.userID,
                typ:         String          = this.typ,
                data:        String          = this.data,
                metadata:    String          = this.metadata,
                description: String          = this.description,
                supplements: Seq[Supplement] = this.supplements,
                comments:    Seq[Comment]    = this.comments) =
    UserWork(id, timestamp, periodID, runID, userID, typ, data, metadata, description, supplements, comments)

}

object UserWork extends FromMapParser {

  import models.datastructure.FullFailValidationList.vsl2Enhanced
  import scalaz.{ Failure, Success, Validation }

  override protected type Target    = UserWork
  override protected type ConsTuple = (Option[Long], Long, String, String, String, String, String, String, String, Seq[Supplement], Seq[Comment])

  override def fromMap(params: MapInput) : Output = {

    val PeriodIDKey    = "period_id"
    val RunIDKey       = "run_id"
    val UserIDKey      = "user_id"
    val DataKey        = "data"
    val Keys           = List(PeriodIDKey, RunIDKey, UserIDKey, DataKey)

    val TypeKey        = "type"
    val MetadataKey    = "metadata"
    val DescriptionKey = "description"

    val valueMaybes = Keys map {
      key => params.get(key) map (Success(_)) getOrElse (Failure("No item with key '%s' passed in\n".format(key))) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ fullFailAppend _) map {
      case periodID :: runID :: userID :: data :: Nil =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (System.currentTimeMillis(), periodID, runID, userID, typ, data, metadata, params.getOrElse(DescriptionKey, ""))
      case _ =>
        throw new IllegalArgumentException("Broken Work validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWork.apply _).tupled

  }

  protected def validate(timestamp: Long, periodID: String, runID: String, userID: String,
                         typ: String, data: String, metadata: String, description: String) : Validation[String, ConsTuple] = {

    val timestampMaybe   = Validator.validateTimestamp(timestamp)
    val periodIDMaybe    = Validator.validatePeriodID(periodID)
    val runIDMaybe       = Validator.validateRunID(runID)
    val userIDMaybe      = Validator.validateUserID(userID)
    val typeMaybe        = Success(typ)
    val dataMaybe        = Success(data)
    val metadataMaybe    = Success(metadata)
    val descriptionMaybe = Success(description)
    val maybes           = List(timestampMaybe, periodIDMaybe, runIDMaybe, userIDMaybe,
                                typeMaybe, dataMaybe, metadataMaybe, descriptionMaybe) map (_ map (List(_)))

    maybes reduce (_ fullFailAppend _) map {
      case (timestamp: Long) :: (periodID: String) :: (runID: String) :: (userID: String) ::
           (typ: String) :: (data: String) :: (metadata: String) :: (description: String) :: Nil =>
        (None, timestamp, periodID, userID, runID, typ, data, metadata, description, Seq(), Seq())
      case _ =>
        throw new IllegalArgumentException("Broken Work constructor validation format!")
    }

  }

}