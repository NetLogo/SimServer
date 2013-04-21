package models.util

import
  java.net.URLEncoder

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 8/29/12
 * Time: 12:59 PM
 */

object NetUtil {
  private val CharEncoding = "UTF-8"
  def encodeForURL(str: String) = encode(str).replaceAllLiterally("+", "%20") // Javaaaaaaaa!!!!  Why do you do this to meeeeeeee?!
  def encode(str: String) = URLEncoder.encode(str, CharEncoding)
}
