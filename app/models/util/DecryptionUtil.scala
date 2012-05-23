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
   * It's not good cryptography... but it should be enough to stop 99.99% of kids from starting HubNet as the teacher
   * @param hash The hashed input
   * @return     (Option[model name], username, teacher name, isTeacher)
   */
  def decodeForHubNet(hash: String) : Validation[String, (Option[String], String, String, Boolean)] = {

    /*
      Ciphering technique (requires that `str.length` < `cipher.length`):
      val str = "thing"
      new String(str.getBytes zip cipher map xor)
    */
    def xor(bytePair: (Byte, Byte)) : Byte = bytePair._1 ^ bytePair._2 toByte
    
    val delim  = ResourceManager(ResourceManager.HubnetDelim)
    val cipher = ResourceManager(ResourceManager.HubNetCipher).getBytes
    val decrypteds = hash.split(delim) map (x => new String(x.getBytes zip cipher map xor)) toList
    
    decrypteds match {
      case username :: teachName :: Nil                         => Success((None, username, teachName, false))
      case username :: teachName :: isTeach :: Nil              => Success((None, username, teachName, isTeach.toBoolean))
      case modelName :: username :: teachName :: isTeach :: Nil => Success((Option(modelName), username, teachName, isTeach.toBoolean))
      case _                                                    => Failure("Failed to decrypt input hash")
    }

  }

}
