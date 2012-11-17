package models.submission

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

object UserWorkComment extends FromMapParser {

  import scalaz.{ Scalaz, ValidationNEL }, Scalaz._

  override protected type Target    = UserWorkComment
  override protected type ConsTuple = (Option[Long], Option[Long], Long, String, String)

  override def fromMap(implicit params: MapInput) : Output = {

    // Required
    val RefIDKey   = "ref_id"
    val UserIDKey  = "user_id"
    val CommentKey = "comment"

    (fetch(RefIDKey) |@| fetch(UserIDKey) |@| fetch(CommentKey)) {
      (_, System.currentTimeMillis(), _, _)
    } flatMap (validate _).tupled map (UserWorkComment.apply _).tupled

  }

  protected def validate(refID: String, timestamp: Long, userID: String, comment: String) : ValidationNEL[FailType, ConsTuple] = {

    val refIDMaybe     = Validator.validateRefID(refID)
    val timestampMaybe = Validator.validateTimestamp(timestamp)
    val userIDMaybe    = Validator.validateUserID(userID)
    val commentMaybe   = comment.successNel[String] flatMap {
      case x if (x.isEmpty) => "Invalid comment; comment cannot be empty".failNel
      case x                => x.successNel[String]
    }

    (refIDMaybe |@| timestampMaybe |@| userIDMaybe |@| commentMaybe) {
      (refID, timestamp, userID: String, comment: String) =>
        (None, Option(refID), timestamp, userID, comment)
    }

  }

}
