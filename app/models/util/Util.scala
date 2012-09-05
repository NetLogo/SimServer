package models.util

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/5/12
 * Time: 5:12 PM
 */

object Util {
  def noneIfEmpty[T <: { def isEmpty: Boolean }](x: T) : Option[T] = if (x.isEmpty) None else Option(x)
}
