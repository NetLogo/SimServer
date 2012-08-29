package models.util

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 8/29/12
 * Time: 12:59 PM
 */

object NetUtil {
  // Javaaaaaaaa!!!!  Why do you do this to meeeeeeee?!
  def encodeForURL(str: String) = java.net.URLEncoder.encode(str, "UTF-8").replaceAllLiterally("+", "%20")
}
