package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 1:53 PM
 */

object KVMatcher {
  private val Matcher = """([^=]+)=(.*)""".r
  def apply(str: String) = { val Matcher(key, value) = str; (key, value) }
  def matches(str: String) = Matcher.pattern.matcher(str).matches()
}
