package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 11:31 AM
 */

import javax.crypto.spec.{PBEKeySpec, PBEParameterSpec}
import javax.crypto.{Cipher, SecretKeyFactory}

import sun.misc.{BASE64Decoder, BASE64Encoder}

sealed trait EncryptionAlgorithm {
  protected def algorithm: String
}

trait PBEWithMF5AndDES extends EncryptionAlgorithm {
  override protected val algorithm = "PBEWithMD5AndDES"
}

abstract class EncryptionUtil(keyPass: String) {

  self: EncryptionAlgorithm =>

  private val Salt    = Array(0xDE, 0x33, 0x10, 0x12, 0xFC, 0x88, 0x4A, 0x90) map (_.toByte)
  private val Iters   = 20

  private val key = SecretKeyFactory.getInstance(algorithm).generateSecret(new PBEKeySpec(keyPass.toCharArray))

  def encrypt(entry: String) : String = {
    val cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.ENCRYPT_MODE, key, new PBEParameterSpec(Salt, Iters))
    new BASE64Encoder().encode(cipher.doFinal(entry.getBytes))
  }

  def decrypt(entry: String) : String = {
    val cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.DECRYPT_MODE, key, new PBEParameterSpec(Salt, Iters))
    new String(cipher.doFinal(new BASE64Decoder().decodeBuffer(entry)))
  }
  
}

