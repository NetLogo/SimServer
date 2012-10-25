package models.parse.submission

import models.submission, submission.UserWork, submission.{ UserWorkComment => Comment }, submission.{ UserWorkSupplement => Supplement }
import models.datastructure.FullFailValidationList.vsl2Enhanced

import scalaz.{ Failure, Success, Validation }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/25/12
 * Time: 4:52 PM
 */

object WorkParser extends ParamParser[UserWork] {

  override protected type ConsTuple = (Option[Long], Long, String, String, String, String, String, String, Seq[Supplement], Seq[Comment])

  override def apply(params: Input) : Output[UserWork] = {

    val PeriodIDKey    = "period_id"
    val RunIDKey       = "run_id"
    val UserIDKey      = "user_id"
    val DataKey        = "data"
    val Keys           = List(PeriodIDKey, RunIDKey, UserIDKey, DataKey)

    val MetadataKey    = "metadata"
    val DescriptionKey = "description"

    val valueMaybes = Keys map {
      key => params.get(key) map (Success(_)) getOrElse (Failure("No item with key '%s' passed in\n".format(key))) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ fullFailAppend _) map {
      case periodID :: runID :: userID :: data :: Nil =>
        (System.currentTimeMillis(), periodID, runID, userID, data, params.getOrElse(MetadataKey, ""), params.getOrElse(DescriptionKey, ""))
      case _ =>
        throw new IllegalArgumentException("Broken Work validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWork.apply _).tupled

  }

  protected def validate(timestamp: Long, periodID: String, runID: String, userID: String,
                         data: String, metadata: String, description: String) : Validation[String, ConsTuple] = {

    val timestampMaybe   = ParamParser.validateTimestamp(timestamp)
    val periodIDMaybe    = ParamParser.validatePeriodID(periodID)
    val runIDMaybe       = ParamParser.validateRunID(runID)
    val userIDMaybe      = ParamParser.validateUserID(userID)
    val dataMaybe        = Success(data)
    val metadataMaybe    = Success(metadata)
    val descriptionMaybe = Success(description)
    val maybes           = List(timestampMaybe, periodIDMaybe, runIDMaybe, userIDMaybe,
                                dataMaybe, metadataMaybe, descriptionMaybe) map (_ map (List(_)))

    maybes reduce (_ fullFailAppend _) map {
      case (timestamp: Long) :: (periodID: String) :: (runID: String) :: (userID: String) ::
             (data: String) :: (metadata: String) :: (description: String) :: Nil =>
        (None, timestamp, periodID, runID, userID, data, metadata, description, Seq(), Seq())
      case _ =>
        throw new IllegalArgumentException("Broken Work constructor validation format!")
    }

  }

}
