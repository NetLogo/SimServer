package models.util

object RequestUtil {

  def extractPropertyFromUri(uri: String, key: String) : Option[String] = {
    try {
      val PropMatcher = """.*\?(.*&)?%s=(.*)(&.*)?""".format(key).r
      val PropMatcher(_, prop, _) = uri
      Option(prop)
    }
    catch {
      case _ => None
    }
  }
  
}
