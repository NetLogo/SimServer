package models.submission

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
                                 comments:    Seq[Comment]) extends Entry {

  def addComments   (newComments:    Comment*)    = this.cloneWith(comments = this.comments ++ newComments)
  def addSupplements(newSupplements: Supplement*) = this.cloneWith(supplements = this.supplements ++ newSupplements)

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

  import scalaz.{ Scalaz, ValidationNEL }, Scalaz._

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
      key => params.get(key) map (_.succeed) getOrElse ("No item with key '%s' passed in".format(key).failNel) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ append _) map {
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
                         typ: String, data: String, metadata: String, description: String) : ValidationNEL[F, ConsTuple] = {

    val timestampMaybe   = Validator.validateTimestamp(timestamp)
    val periodIDMaybe    = Validator.validatePeriodID(periodID)
    val runIDMaybe       = Validator.validateRunID(runID)
    val userIDMaybe      = Validator.validateUserID(userID)
    val typeMaybe        = typ.succeed
    val dataMaybe        = data.succeed
    val metadataMaybe    = metadata.succeed
    val descriptionMaybe = description.succeed
    val maybes           = List(timestampMaybe, periodIDMaybe, runIDMaybe, userIDMaybe,
                                typeMaybe, dataMaybe, metadataMaybe, descriptionMaybe) map (_ map (List(_)))

    maybes reduce (_ append _) map {
      case (timestamp: Long) :: (periodID: String) :: (runID: String) :: (userID: String) ::
           (typ: String) :: (data: String) :: (metadata: String) :: (description: String) :: Nil =>
        (None, timestamp, periodID, runID, userID, typ, data, metadata, description, Seq(), Seq())
      case _ =>
        throw new IllegalArgumentException("Broken Work constructor validation format!")
    }

  }

}
