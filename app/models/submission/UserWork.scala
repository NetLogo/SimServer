package models.submission

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz._

import
  models.{ submission, util },
    submission.{ UserWorkComment => Comment, UserWorkSupplement => Supplement },
    util.ParamBundle

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
                                 rawData:     Array[Byte],
                                 metadata:    String,
                                 description: String,
                                 supplements: Seq[Supplement],
                                 comments:    Seq[Comment]) extends Entry {
  def addComments   (newComments:    Comment*)    = this.copy(comments = this.comments ++ newComments)
  def addSupplements(newSupplements: Supplement*) = this.copy(supplements = this.supplements ++ newSupplements)
}

object UserWork extends FromMapParser with DataFromBundleParser {

  override protected type Target      = UserWork
  override protected type ConsTuple   = (Option[Long], Long, String, String, String, String, String, Array[Byte], String, String, Seq[Supplement], Seq[Comment])
  override protected type ParsedTuple = (String, String, String, String, Array[Byte], String, String)

  override protected def fromBundleHelper(bundle: ParamBundle) : Output = {
    constructFrom(parseFromMap(bundle.stringParams) flatMap {
      case (run, period, user, typ, rawData, metadata, description) if (rawData.isEmpty) =>
        byteFetch(DataKey)(bundle.byteParams) map {
          realRawData => (run, period, user, typ, realRawData, metadata, description)
        }
      case x =>
        x.successNel[FailType]
    })
  }

  override protected def parseFromMap(implicit params: MapInput) : Parsed = {

    val RunIDKey       = "run_id"
    val PeriodIDKey    = "period_id"
    val UserIDKey      = "user_id"

    // Not required
 // val DataKey
    val TypeKey        = "type"
    val MetadataKey    = "metadata"
    val DescriptionKey = "description"

    // Builds an applicative, and constructs a tuple out of the applied input
    // The function passed to the applicative is not run is one or more `Failure`s is in the created applicative
    // All `Failure`s coalesce in a `NonEmptyList`, and the code will essentially short-circuit to avoid the "success" path
    (fetch(RunIDKey) |@| fetch(PeriodIDKey) |@| fetch(UserIDKey)) {
      (runID, periodID, userID) =>
        val metadata = params.getOrElse(MetadataKey, tryHarderToGetNested(MetadataKey))
        val typ      = params.getOrElse(TypeKey, Metadata.fromString(metadata).fold((_ => ""), (_.getType)))
        val rawData  = params.getOrElse(DataKey, "").getBytes
        (runID, periodID, userID, typ, rawData, metadata, params.getOrElse(DescriptionKey, ""))
    }

  }

  protected def validate(runID: String, periodID: String, userID: String,
                         typ: String, rawData: Array[Byte], metadata: String, description: String) : ValidationNel[FailType, ConsTuple] = {

    val runIDMaybe       = Validator.validateRunID(runID)
    val periodIDMaybe    = Validator.validatePeriodID(periodID)
    val userIDMaybe      = Validator.validateUserID(userID)
    val typeMaybe        = Validator.accept(typ)
    val rawMaybe         = Validator.accept(rawData)
    val metadataMaybe    = Validator.accept(metadata)
    val descriptionMaybe = Validator.accept(description)

    (runIDMaybe |@| periodIDMaybe |@| userIDMaybe |@| typeMaybe |@| rawMaybe |@| metadataMaybe |@| descriptionMaybe) {
      (None, System.currentTimeMillis, _, _, _, _, "", _, _, _, Seq(), Seq())
    }

  }

  override protected def constructFrom(parsed: Parsed) : Output =
    parsed flatMap (validate _).tupled map (UserWork.apply _).tupled


}
