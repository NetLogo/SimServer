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
                                 runID:       String,
                                 periodID:    String,
                                 userID:      String,
                                 typ:         String,
                                 data:        String,
                                 metadata:    String,
                                 description: String,
                                 supplements: Seq[Supplement],
                                 comments:    Seq[Comment]) extends Entry {
  def addComments   (newComments:    Comment*)    = this.copy(comments = this.comments ++ newComments)
  def addSupplements(newSupplements: Supplement*) = this.copy(supplements = this.supplements ++ newSupplements)
}

object UserWork extends FromMapParser {

  import scalaz.{ Scalaz, ValidationNEL }, Scalaz._

  override protected type Target    = UserWork
  override protected type ConsTuple = (Option[Long], Long, String, String, String, String, String, String, String, Seq[Supplement], Seq[Comment])

  override def fromMap(implicit params: MapInput) : Output = {

    val RunIDKey       = "run_id"
    val PeriodIDKey    = "period_id"
    val UserIDKey      = "user_id"
    val DataKey        = "data"

    // Not required
    val TypeKey        = "type"
    val MetadataKey    = "metadata"
    val DescriptionKey = "description"

    // Builds an applicative, and constructs a tuple out of the applied input
    // The function passed to the applicative is not run is one or more `Failure`s is in the created applicative
    // All `Failure`s coalesce in a `NonEmptyList`, and the code will essentially short-circuit to avoid the "success" path
    val valueTupleMaybe = (fetch(RunIDKey) |@| fetch(PeriodIDKey) |@| fetch(UserIDKey) |@| fetch(DataKey)) {
      (runID, periodID, userID, data) =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (System.currentTimeMillis(), runID, periodID, userID, typ, data, metadata, params.getOrElse(DescriptionKey, ""))
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWork.apply _).tupled

  }

  protected def validate(timestamp: Long, runID: String, periodID: String, userID: String,
                         typ: String, data: String, metadata: String, description: String) : ValidationNEL[FailType, ConsTuple] = {

    val timestampMaybe   = Validator.validateTimestamp(timestamp)
    val runIDMaybe       = Validator.validateRunID(runID)
    val periodIDMaybe    = Validator.validatePeriodID(periodID)
    val userIDMaybe      = Validator.validateUserID(userID)
    val typeMaybe        = Validator.accept(typ)
    val dataMaybe        = Validator.accept(data)
    val metadataMaybe    = Validator.accept(metadata)
    val descriptionMaybe = Validator.accept(description)

    (timestampMaybe |@| runIDMaybe |@| periodIDMaybe |@| userIDMaybe |@| typeMaybe |@| dataMaybe |@| metadataMaybe |@| descriptionMaybe) {
      (None, _, _, _, _, _, _, _, _, Seq(), Seq())
    }

  }

}
