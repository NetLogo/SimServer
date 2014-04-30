package models.hubnet

import
  org.apache.commons.codec.binary.Base64

import
  scalaz.{ Scalaz, ValidationNel },
    Scalaz.ToValidationV

import
  models.crypto._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:17 PM
 */

object HubNetServerRegistry extends CanManageHubNetRegistries {

  override protected type RegistryValue      = RegistryBundle
  override protected type RegistrationReturn = (String, String)

  override def registerTeacher(teacherName: String): (String, String) = {
    val bundle       = registryMap.getOrElse(teacherName, RegistryBundle(Seq(), None))
    val cryptoBundle = new CryptoManager with RSA with K2048
    registryMap += teacherName -> bundle.copy(cryptos = cryptoBundle +: bundle.cryptos)
    (cryptoBundle.publicModulus.toString, cryptoBundle.publicExponent.toString)
  }

  override def registerLookupAddress(teacherName: String, data: String): Unit = {

    def findFirstThatDecrypts(cryptos: Seq[CryptoManager], data: Array[Byte]): Option[String] =
      cryptos.foldLeft(Option[String](null)) {
        case (acc, x) =>
          if (acc.isEmpty) {
            try Option(x.decrypt(data))
            catch { case ex: Exception => None }
          }
          else
            acc
      }

    val encryptedData = Base64.decodeBase64(data.getBytes)

    registryMap.get(teacherName) foreach {
      case RegistryBundle(cryptos, _) => // This is kinda lens-y!
        findFirstThatDecrypts(cryptos, encryptedData) foreach {
          decrypted =>
            val Seq(ip, port) = decrypted.split(":").toSeq
            registryMap += teacherName -> RegistryBundle(cryptos, Option(LookupAddress(ip, port.toInt)))
            expiryManager(teacherName)
        }
    }

  }

  override def getIPAndPortByTeacherName(teacherName: String): ValidationNel[String, (String, Int)] =
    registryMap.get(teacherName) map {
      case RegistryBundle(_, Some(LookupAddress(ip, port))) =>
        (ip, port).successNel[String]
      case _ =>
        NotStartedFormat(teacherName).failNel
    } getOrElse NotFoundFormat(teacherName).failNel

  protected case class RegistryBundle(cryptos: Seq[CryptoManager], address: Option[LookupAddress])

}

