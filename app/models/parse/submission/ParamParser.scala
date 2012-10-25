package models.parse.submission

import models.submission.Submission

import scalaz.{ Failure, Success, Validation }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/25/12
 * Time: 4:32 PM
 */

// Unless I wanna go all `shapeless` on this thing's ass, there's not really a good way to enforce having a `validate` method... --JAB
private[submission] trait ParamParser[T <: Submission] {

  protected type ConsTuple
  protected type Input     = Map[String, String]
  protected type Output[U] = Validation[String, U]

  def apply(params: Input) : Output[T]

}

private[submission] object ParamParser {

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