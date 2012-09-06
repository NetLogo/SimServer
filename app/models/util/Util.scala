package models.util

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/5/12
 * Time: 5:12 PM
 */

object Util {

  // Has to be two methods, because the compiler gets touchy about inferring `U` from a default parameter
  def noneIfEmpty[T <% { def isEmpty: Boolean }](x: T) : Option[T] = noneIfEmpty(x, identity[T] _)
  def noneIfEmpty[T <% { def isEmpty: Boolean }, U](x: T, transformer: T => U) : Option[U] = if (x.isEmpty) None else Option(transformer(x))

  def ifFirstWrapSecond[T](x: => Boolean, y: => T) : Option[T] = if (x) Some(y) else None

}
