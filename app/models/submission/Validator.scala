package models.submission

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz.ToValidationV

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:17 PM
 */

private[submission] object Validator {

  protected type Fail = String
  protected type V[T] = ValidationNel[Fail, T]

  def accept[T](x: T) = x.successNel[Fail]
  def deny  [T](x: T) = x.failNel

  def validateRefID(refID: String): V[Long] =
    ensureNotEmpty(refID, "ref ID") flatMap {
      x =>
        try x.toLong match { case y => accept(y) }
        catch {
          case ex: NumberFormatException => deny(s"Cannot convert '$x' to Long; either not numerical or too many digits.")
        }
    }

  def validateUserID    (userID: String) = ensureNotEmpty(userID,   "user ID")
  def validatePeriodID(periodID: String) = ensureNotEmpty(periodID, "period ID")
  def validateRunID      (runID: String) = ensureNotEmpty(runID,    "run ID")

  protected val ErrorMessageTemplate = "Invalid value given for %s; %s".format(_: String, _: String)

  def ensure[T](data: T, dataName: String)(errorDesc: String)(errorCond: (T) => Boolean): V[T] = {
    lazy val errorMessage = ErrorMessageTemplate(dataName, errorDesc)
    failUnderCond(data, errorCond, errorMessage)
  }

  def ensureNotEmpty[T <% { def isEmpty: Boolean }](data: T, dataName: String): V[T] =
    ensure(data, dataName)("cannot be empty")(_.isEmpty)

  def ensureNonNegative[T <% AnyVal { def <=(x: Int): Boolean }](data: T, dataName: String): V[T] =
    ensure(data, dataName)("value is too small")(_ <= 0)

  protected def failUnderCond[T](param: T, failureCond: (T) => Boolean, errorStr: => String): V[T] = param match {
    case x if failureCond(x) => deny(errorStr)
    case x                   => accept(x)
  }

}

