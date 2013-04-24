package models.crypto

import
  java.security.{ KeyPair, KeyPairGenerator, PublicKey, PrivateKey, spec },
    spec.KeySpec

import
  javax.crypto.Cipher

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 11:31 AM
 */

trait PublicKeyCrypto {

  self: CryptoAlgorithm with CryptoManager with SizedKey =>

  protected type Spec <: KeySpec

  protected lazy val KeyPair(publicKey, privateKey) = {
    val kpg = KeyPairGenerator.getInstance(algorithm)
    kpg.initialize(keySize)
    kpg.genKeyPair()
  }

  object KeyPair {
    def unapply(keyPair: KeyPair) : Option[(PublicKey, PrivateKey)] = Option((keyPair.getPublic, keyPair.getPrivate))
  }

  override protected def encryptCipherInit = (cipher: Cipher) => cipher.init(Cipher.ENCRYPT_MODE, publicKey)
  override protected def decryptCipherInit = (cipher: Cipher) => cipher.init(Cipher.DECRYPT_MODE, privateKey)

}

trait CryptoManager {

  self: CryptoAlgorithm =>

  // Different algorithms call different overloaded versions of `cipher.init`
  protected def encryptCipherInit : (Cipher) => Unit
  protected def decryptCipherInit : (Cipher) => Unit

  final def encrypt(entry: String) : String = {
    performCrypto(entry.getBytes("UTF-8")) {
      encryptCipherInit(_)
    }
  }

  final def decrypt(entry: Array[Byte]) : String = {
    performCrypto(entry) {
      decryptCipherInit(_)
    }
  }

  private def performCrypto(entry: Array[Byte])(cipherInit: (Cipher) => Unit) : String = {
    val cipher = Cipher.getInstance(algorithm)
    cipherInit(cipher)
    new String(cipher.doFinal(entry))
  }

}

