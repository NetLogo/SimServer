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

  override def fromMap(params: MapInput) : Output = {

    val RefIDKey   = "ref_id"
    val UserIDKey  = "user_id"
    val CommentKey = "comment"
    val Keys       = List(RefIDKey, UserIDKey, CommentKey)

    val valueMaybes = Keys map {
      key => params.get(key) map (_.succeed) getOrElse ("No item with key '%s' passed in".format(key).failNel) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ append _) map {
      case refID :: userID :: comment :: Nil => (refID, System.currentTimeMillis(), userID, comment)
      case _                                 => throw new IllegalArgumentException("Broken Comment validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWorkComment.apply _).tupled

  }

  protected def validate(refID: String, timestamp: Long, userID: String, comment: String) : ValidationNEL[F, ConsTuple] = {

    val refIDMaybe     = Validator.validateRefID(refID)
    val timestampMaybe = Validator.validateTimestamp(timestamp)
    val userIDMaybe    = Validator.validateUserID(userID)
    val commentMaybe   = comment.succeed flatMap {
      case x if (x.isEmpty) => "Invalid comment; comment cannot be empty".failNel
      case x                => x.succeed
    }

    val maybes = List(refIDMaybe, timestampMaybe, userIDMaybe, commentMaybe) map (_ map (List(_)))

    maybes reduce (_ append _) map {
      case (refID: Long) :: (timestamp: Long) :: (userID: String) :: (comment: String) :: Nil =>
        (None, Option(refID), timestamp, userID, comment)
      case _ =>
        throw new IllegalArgumentException("Broken Comment constructor validation format!")
    }

  }

}
