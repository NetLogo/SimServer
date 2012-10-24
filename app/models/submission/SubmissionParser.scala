package models.submission

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 3:44 PM
 */

object SubmissionParser {

  private type Input     = Map[String, String]
  private type Output[T] = Option[T]

  def parseOutUserWork(params: Input) : Output[UserWork] = {

    val PeriodIDKey    = "period_id"
    val RunIDKey       = "run_id"
    val UserIDKey      = "user_id"
    val DataKey        = "data"
    val MetaDataKey    = "meta"
    val DescriptionKey = "description"

    for {
      periodID    <- params.get(PeriodIDKey)
      runID       <- params.get(RunIDKey)
      userID      <- params.get(UserIDKey)
      data        <- params.get(DataKey)
      metaData    <- params.get(MetaDataKey)
      description <- params.get(DescriptionKey)
    } yield {
      UserWork(None, System.currentTimeMillis(), periodID, runID, userID, data, metaData, description, Seq(), Seq())
    }

  }

  def parseOutWorkComment(params: Input) : Output[UserWorkComment] = {

    val RefIDKey   = "ref_id"
    val UserIDKey  = "user_id"
    val CommentKey = "comment"

    for {
      refID   <- params.get(RefIDKey)
      userID  <- params.get(UserIDKey)
      comment <- params.get(CommentKey)
    } yield {
      UserWorkComment(None, Option(refID.toLong), System.currentTimeMillis(), userID, comment)
    }

  }

  def parseOutWorkSupplement(params: Input) : Output[UserWorkSupplement] = {

    val RefIDKey    = "ref_id"
    val TypeKey     = "type"
    val DataKey     = "data"
    val MetadataKey = "metadata"

    for {
      refID    <- params.get(RefIDKey)
      data     <- params.get(DataKey)
      metadata <- params.get(MetadataKey)
    } yield {
      UserWorkSupplement(None, Option(refID.toLong), params.get(TypeKey) getOrElse (SupplementMetadataParser(metadata).getType), data, metadata)
    }

  }

}
