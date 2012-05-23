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

  val HubnetDelim = "hubnet_delim"
  val HubNetCipher = "hubnet_cipher"

  private lazy val infoMap = readAccessInfo()

  def apply(s: String) : String         = infoMap(s)
  def get  (s: String) : Option[String] = infoMap.get(s)

  private def readAccessInfo() : Map[String, String] = {
    try {
      val src  = io.Source.fromFile(AccessFileName)
      try {
        val KVMatcher = """([^=]*)=([^=]*)""".r
        src.getLines().toIndexedSeq filter (KVMatcher.pattern.matcher(_) matches()) map { line => val KVMatcher(k, v) = line; (k, v) } toMap
      }
      finally {
        src.close()
      }
    }
    catch {
      case ex =>
        Logger.error("Failed to read access info from file", ex)
        Map[String, String]()
    }
  }

}
