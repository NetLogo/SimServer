package models.util

import scalaz.{Failure, Success, Validation}

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 4:47 PM
 */

object DecryptionUtil {

  /**
   * This should be enough to stop 99.9999% of kids from starting HubNet as the teacher...
   * Assumes that the other side is using the same `EncryptionUtil` and resources file/key password
   * @param encryptedStr The encrypted input
   * @return            (Option[model name], username, teacher name, isTeacher)
   */
  def decodeForHubNet(encryptedStr: String) : Validation[String, (Option[String], String, String, Boolean)] = {

    val keyPass    = ResourceManager(ResourceManager.HubNetKeyPass)
    val delim      = ResourceManager(ResourceManager.HubnetDelim)
    val decrypteds = (new EncryptionUtil(keyPass) with PBEWithMF5AndDES) decrypt encryptedStr split delim toList
    
    decrypteds match {
      case username :: teachName :: Nil                         => Success((None, username, teachName, false))
      case username :: teachName :: isTeach :: Nil              => Success((None, username, teachName, isTeach.toBoolean))
      case modelName :: username :: teachName :: isTeach :: Nil => Success((Option(modelName), username, teachName, isTeach.toBoolean))
      case _                                                    => Failure("Failed to interpret input")
    }

  }

}
