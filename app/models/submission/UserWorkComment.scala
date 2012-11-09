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

  import models.datastructure.FullFailValidationList.vsl2Enhanced
  import scalaz.{ Failure, Success, Validation }

  override protected type Target    = UserWorkComment
  override protected type ConsTuple = (Option[Long], Option[Long], Long, String, String)

  override def fromMap(params: MapInput) : Output = {

    val RefIDKey   = "ref_id"
    val UserIDKey  = "user_id"
    val CommentKey = "comment"
    val Keys       = List(RefIDKey, UserIDKey, CommentKey)

    val valueMaybes = Keys map {
      key => params.get(key) map (Success(_)) getOrElse (Failure("No item with key '%s' passed in\n".format(key))) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ fullFailAppend _) map {
      case refID :: userID :: comment :: Nil => (refID, System.currentTimeMillis(), userID, comment)
      case _                                 => throw new IllegalArgumentException("Broken Comment validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWorkComment.apply _).tupled

  }

  protected def validate(refID: String, timestamp: Long, userID: String, comment: String) : Validation[String, ConsTuple] = {

    val refIDMaybe     = Validator.validateRefID(refID)
    val timestampMaybe = Validator.validateTimestamp(timestamp)
    val userIDMaybe    = Validator.validateUserID(userID)
    val commentMaybe   = Success(comment) flatMap {
      case x if (x.isEmpty) => Failure("Invalid comment; comment cannot be empty")
      case x                => Success(x)
    }

    val maybes = List(refIDMaybe, timestampMaybe, userIDMaybe, commentMaybe) map (_ map (List(_)))

    maybes reduce (_ fullFailAppend _) map {
      case (refID: Long) :: (timestamp: Long) :: (userID: String) :: (comment: String) :: Nil =>
        (None, Option(refID), timestamp, userID, comment)
      case _ =>
        throw new IllegalArgumentException("Broken Comment constructor validation format!")
    }

  }}
