package models.submission

import scalaz.Scalaz.ToValidationV
import scalaz.ValidationNEL

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:16 PM
 */

// Unless I wanna go all `shapeless` on this thing's ass, there's not really a good way to enforce having a `validate` method... --JAB
private[submission] trait Parser {

  protected type F = String
  implicit def any2ValidationP[T](that: T) = new ValidationP(that)
  protected class ValidationP[T](that: T) { def succeed = that.successNel[F] }

  protected type Target
  protected type ConsTuple
  protected type Output = ValidationNEL[F, Target]

}

private[submission] trait FromMapParser extends Parser {
  protected type MapInput = Map[String, String]
  def fromMap(params: MapInput) : Output
}

private[submission] trait FromStringParser extends Parser {
  def fromString(str: String) : Output
}
