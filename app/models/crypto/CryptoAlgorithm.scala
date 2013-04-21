package models.crypto

import
  java.security.{ KeyFactory, spec },
    spec.RSAPublicKeySpec

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 4/21/13
 * Time: 3:43 PM
 */

sealed trait CryptoAlgorithm {
  protected def algorithm: String
}

trait RSA extends CryptoAlgorithm with PublicKeyCrypto {

  self: CryptoManager with SizedKey =>

  override protected type Spec = RSAPublicKeySpec

  override protected val algorithm = "RSA"

  lazy val (publicModulus, publicExponent) = {
    val spec = KeyFactory.getInstance(algorithm).getKeySpec(publicKey, classOf[Spec])
    (spec.getModulus, spec.getPublicExponent)
  }

}

