package models.util

import scalaz.{ Scalaz, ValidationNEL }, Scalaz.ToValidationV

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 4:47 PM
 */

object DecryptionUtil {

  // This should be enough to stop 99.9999% of kids from starting HubNet as the teacher...
  // Assumes that the other side is using the same `EncryptionUtil` and resources file/key password
  def decodeForHubNet(encryptedStr: String, isTeacher: Boolean) : ValidationNEL[String, HubNetSettings] = {
    val decrypteds = decodeToMap(encryptedStr, ResourceManager(ResourceManager.HubNetKeyPass), ResourceManager(ResourceManager.HubnetDelim))
    HubNetSettings(decrypteds, isTeacher) map (_.successNel[String]) getOrElse "Failed to interpret input".failNel
  }

  private def decodeToMap(encryptedStr: String, keyPass: String, delim: String) : Map[String, String] = {
    (new EncryptionUtil(keyPass) with PBEWithMF5AndDES) decrypt(encryptedStr) split(delim replaceAllLiterally("|", "\\|")) map (KVMatcher(_)) toMap
  }

}
