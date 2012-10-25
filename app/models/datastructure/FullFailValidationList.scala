package models.datastructure

import scalaz.{ Failure, Success, Validation }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/25/12
 * Time: 5:56 PM
 */

// Oh, god... I dug _this thing_ up....  --JAB (10/25/12)
object FullFailValidationList {

  type VSList[T] = Validation[String, List[T]]

  implicit def vsl2Enhanced[T](v: VSList[T]) : FullFailValidationList[T] = new FullFailValidationList(v)

  class FullFailValidationList[T](v: VSList[T]) {
    def fullFailAppend(validation: VSList[T]) : VSList[T] = (v, validation) match {
      case (Success(s1), Success(s2)) => Success(s1 ++ s2)
      case (Success(s),  Failure(f))  => Failure(f)
      case (Failure(f),  Success(s))  => Failure(f)
      case (Failure(f1), Failure(f2)) => Failure(f1 + f2)
    }
  }

}
