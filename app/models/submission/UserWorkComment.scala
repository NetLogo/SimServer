package models.submission

import
  models.util.ParamBundle

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:32 PM
 */

case class UserWorkComment(override val id:        Option[Long],
                           override val refID:     Option[Long],
                           timestamp: Long = System.currentTimeMillis(),
                           userID:    String,
                           comment:   String) extends Association

object UserWorkComment extends FromMapParser with FromBundleParser {

  import scalaz.{ Scalaz, ValidationNel }, Scalaz._

  override protected type Target      = UserWorkComment
  override protected type ConsTuple   = (Option[Long], Option[Long], Long, String, String)
  override protected type ParsedTuple = (String, String, String)

  override def fromBundle(bundle: ParamBundle) : Output =
    fromMap(bundle.stringParams)

  override def parseFromMap(implicit params: MapInput) : Parsed = {

    // Required
    val RefIDKey   = "ref_id"
    val UserIDKey  = "user_id"
    val CommentKey = "comment"

    (fetch(RefIDKey) |@| fetch(UserIDKey) |@| fetch(CommentKey)) {
      (_, _, _)
    }

  }

  protected def validate(refID: String, userID: String, comment: String) : ValidationNel[FailType, ConsTuple] = {

    val refIDMaybe     = Validator.validateRefID(refID)
    val userIDMaybe    = Validator.validateUserID(userID)
    val commentMaybe   = Validator.ensureNotEmpty(comment, "comment")

    (refIDMaybe |@| userIDMaybe |@| commentMaybe) {
      (refID, userID: String, comment: String) =>
        (None, Option(refID), System.currentTimeMillis, userID, comment)
    }

  }

  override protected def constructFrom(parsed: Parsed) : Output =
    parsed flatMap (validate _).tupled map (UserWorkComment.apply _).tupled

}
