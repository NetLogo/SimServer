package models.submission

import scalaz.{ Failure, Success, Validation }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:17 PM
 */

private[submission] object Validator {

  def validateRefID(refID: String) : Validation[String, Long] =
    failUnderCond(refID, StringEmptyCond, "Invalid value given for ref ID; ref ID cannot be empty") flatMap {
      x =>
        try {
          Success(x.toLong)
        }
        catch {
          case ex: NumberFormatException => Failure("Cannot convert '%s' to Long; either not numerical or too many digits.\n".format(x))
        }
    }

  def validateTimestamp(timestamp: Long) = failUnderCond(timestamp, LongLTEZeroCond, "Invalid timestamp; value is too small")
  def validatePeriodID(periodID: String) = failUnderCond(periodID,  StringEmptyCond, "Invalid period ID; period ID cannot be empty")
  def validateRunID      (runID: String) = failUnderCond(runID,     StringEmptyCond, "Invalid run ID; run ID cannot be empty")
  def validateUserID    (userID: String) = failUnderCond(userID,    StringEmptyCond, "Invalid username; username cannot be empty")

  protected val StringEmptyCond = (_: String).isEmpty
  protected val LongLTEZeroCond = (_: Long) <= 0

  protected def failUnderCond[T](param: T, cond: (T) => Boolean, errorStr: String) : Validation[String, T] = Success(param) flatMap {
    case x if cond(x) => Failure(errorStr + "\n")
    case x            => Success(x)
  }

}

