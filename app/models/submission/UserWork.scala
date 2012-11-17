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

  override def fromMap(implicit params: MapInput) : Output = {

    val PeriodIDKey    = "period_id"
    val RunIDKey       = "run_id"
    val UserIDKey      = "user_id"
    val DataKey        = "data"

    // Not required
    val TypeKey        = "type"
    val MetadataKey    = "metadata"
    val DescriptionKey = "description"

    // Builds an applicative, and constructs a tuple out of the applied input
    // The function passed to the applicative is not run is one or more `Failure`s is in the created applicative
    // All `Failure`s coalesce in a `NonEmptyList`, and the code will essentially short-circuit to avoid the "success" path
    val valueTupleMaybe = (fetch(PeriodIDKey) |@| fetch(RunIDKey) |@| fetch(UserIDKey) |@| fetch(DataKey)) {
      (periodID, runID, userID, data) =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (System.currentTimeMillis(), periodID, runID, userID, typ, data, metadata, params.getOrElse(DescriptionKey, ""))
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWork.apply _).tupled

  }

  protected def validate(timestamp: Long, periodID: String, runID: String, userID: String,
                         typ: String, data: String, metadata: String, description: String) : ValidationNEL[FailType, ConsTuple] = {

    val timestampMaybe   = Validator.validateTimestamp(timestamp)
    val periodIDMaybe    = Validator.validatePeriodID(periodID)
    val runIDMaybe       = Validator.validateRunID(runID)
    val userIDMaybe      = Validator.validateUserID(userID)
    val typeMaybe        = typ.successNel[String]
    val dataMaybe        = data.successNel[String]
    val metadataMaybe    = metadata.successNel[String]
    val descriptionMaybe = description.successNel[String]

    (timestampMaybe |@| periodIDMaybe |@| runIDMaybe |@| userIDMaybe |@| typeMaybe |@| dataMaybe |@| metadataMaybe |@| descriptionMaybe) {
      (None, _, _, _, _, _, _, _, _, Seq(), Seq())
    }

  }

}
