package models.hubnet

import
  scala.collection.mutable.{ Map => MMap }

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

object HubNetServerRegistry {

  private val NotFoundFormat   = "Teacher '%s' has not attempted to start any HubNet servers recently.".format(_: String)
  private val NotStartedFormat = "There is no existing HubNet server for teacher '%s'.  Please ask your teacher to connect to the activity and then try again.\n".format(_: String)

  private val registryMap   = MMap[String, RegistryBundle]()
  private val expiryManager = new ExpiryManager(removeEntry _, "hubnet-registry")

  def registerTeacher(teacherName: String): (String, String) = {
    val bundle       = registryMap.getOrElse(teacherName, RegistryBundle(Seq(), None))
    val cryptoBundle = new CryptoManager with RSA with K2048
    registryMap += teacherName -> bundle.copy(cryptos = cryptoBundle +: bundle.cryptos)
    (cryptoBundle.publicModulus.toString, cryptoBundle.publicExponent.toString)
  }

  def registerLookupAddress(teacherName: String, encryptedData: Array[Byte]): Unit = {

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

  def getPortByTeacherName(teacherName: String): ValidationNel[String, (String, Int)] =
    registryMap.get(teacherName) map {
      case RegistryBundle(_, Some(LookupAddress(ip, port))) =>
        (ip, port).successNel[String]
      case _ =>
        NotStartedFormat(teacherName).failNel
    } getOrElse NotFoundFormat(teacherName).failNel

  private def removeEntry(teacherName: String): Unit = {
    registryMap -= teacherName
  }

}

private case class LookupAddress(ip: String, port: Int)
private case class RegistryBundle(cryptos: Seq[CryptoManager], address: Option[LookupAddress])

