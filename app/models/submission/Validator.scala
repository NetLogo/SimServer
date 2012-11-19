package models.submission

import scalaz.{ Scalaz, ValidationNEL }, Scalaz.ToValidationV

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:17 PM
 */

private[submission] object Validator {

  protected type V[T] = ValidationNEL[String, T]

  def validateRefID(refID: String) : V[Long] =
    ensureNotEmpty(refID, "ref ID") flatMap {
      x =>
        try x.toLong.successNel
        catch {
          case ex: NumberFormatException => "Cannot convert '%s' to Long; either not numerical or too many digits.".format(x).failNel
        }
    }

  def validateTimestamp(timestamp: Long) = ensureNonNegative(timestamp, "timestamp")
  def validateUserID    (userID: String) = ensureNotEmpty(userID,   "user_id")
  def validatePeriodID(periodID: String) = ensureNotEmpty(periodID, "period id")
  def validateRunID      (runID: String) = ensureNotEmpty(runID,    "run id")

  protected val ErrorMessageTemplate = "Invalid value given for %s; %s".format(_: String, _: String)

  def ensure[T](data: T, dataName: String)(errorDesc: String)(errorCond: (T) => Boolean) : V[T] = {
    lazy val errorMessage = ErrorMessageTemplate(dataName, errorDesc)
    failUnderCond(data, errorCond, errorMessage)
  }

  def ensureNotEmpty[T <% { def isEmpty : Boolean }](data: T, dataName: String) : V[T] =
    ensure(data, dataName)("cannot be empty")(_.isEmpty)

  def ensureNonNegative[T <% AnyVal { def <=(x: Int) : Boolean }](data: T, dataName: String) : V[T] =
    ensure(data, dataName)("value is too small")(_ <= 0)

  protected def failUnderCond[T](param: T, cond: (T) => Boolean, errorStr: => String) : V[T] = param match {
    case x if cond(x) => errorStr.failNel
    case x            => x.successNel
  }

}

