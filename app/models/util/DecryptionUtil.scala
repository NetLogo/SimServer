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
   * @return            (Option[model name], isHeadless, username, teacher name, isTeacher)
   */
  def decodeForHubNet(encryptedStr: String) : Validation[String, (Option[String], String, Boolean, String, Boolean)] = {

    val keyPass    = ResourceManager(ResourceManager.HubNetKeyPass)
    val delim      = ResourceManager(ResourceManager.HubnetDelim)
    val decrypteds = (new EncryptionUtil(keyPass) with PBEWithMF5AndDES) decrypt encryptedStr split (delim replaceAllLiterally("|", "\\|")) toList

    // This suuuuuuuuucks!  I foresee encryption with key-value pairs in the near future...
    decrypteds match {

      case              username :: teachName            :: Nil => Success((None,              username, false, teachName, false))
      case              username :: teachName :: "false" :: Nil => Success((None,              username, false, teachName, false))
      case modelName :: username :: teachName :: "true"  :: Nil => Success((Option(modelName), username, false, teachName, true))

      case              username :: "false" :: teachName            :: Nil => Success((None,              username, false, teachName, false))
      case              username :: "false" :: teachName :: "false" :: Nil => Success((None,              username, false, teachName, false))
      case modelName :: username :: "false" :: teachName :: "true"  :: Nil => Success((Option(modelName), username, false, teachName, true))

      case              username :: "true" :: teachName            :: Nil => Success((None,              username, true, teachName, false))
      case              username :: "true" :: teachName :: "false" :: Nil => Success((None,              username, true, teachName, false))
      case modelName :: username :: "true" :: teachName :: "true"  :: Nil => Success((Option(modelName), username, true, teachName, true))

      case _ => Failure("Failed to interpret input")

    }

  }

}
