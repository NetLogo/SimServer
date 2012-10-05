package models.util

import play.api.Logger

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 5:36 PM
 */

object ResourceManager {

  private val AccessFileName = "info_file"

  val HubNetKeyPass = "hubnet_key_pass"
  val HubnetDelim   = "hubnet_delim"

  private lazy val infoMap = readAccessInfo()

  def apply(s: String) : String         = infoMap(s)
  def get  (s: String) : Option[String] = infoMap.get(s)

  private def readAccessInfo() : Map[String, String] = {
    try {
      val src  = io.Source.fromFile(AccessFileName)
      try {
        src.getLines().toSeq filter (KVMatcher.matches(_)) map (KVMatcher(_)) toMap
      }
      finally {
        src.close()
      }
    }
    catch {
      case ex: Exception =>
        Logger.error("Failed to read access info from file", ex) // One of the few errors in this system that should be log level "error"
        Map[String, String]()
    }
  }

}
