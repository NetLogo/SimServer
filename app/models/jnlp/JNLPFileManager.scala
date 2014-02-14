package models.jnlp

import
  java.io.File

import
  scala.concurrent.duration._

import
  akka.{ actor, pattern },
    actor._,
    pattern.ask

import
  models.filemanager.FileManager

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 2/22/13
 * Time: 3:59 PM
 */

object JNLPFileManager extends FileManager {

            override lazy val MyFolderName = "gen"
  protected override lazy val LifeSpan     = 1000 days
  protected override lazy val SystemName   = "JNLPFiles"

  private val writer = system.actorOf(Props[Writer])

  // Don't sign any applications external to us! --JAB (11/12/13)
  def registerAndSign(jnlp: JNLP, uuid: String, defaultCodebase: String): String = {
    val msg =
      if (defaultCodebase == jnlp.codebaseURI.toString)
        Sign(jnlp, uuid)
      else
        Register(jnlp, uuid)
    writer ! msg
    genPath(genFilename(uuid))
  }

  def genFilename(basis: String): String =
    s"$MyFolderName/${basis}.jnlp"

  private case class Register(jnlp: JNLP, uuid: String)
  private case class Sign    (jnlp: JNLP, uuid: String)

  private class Writer extends Actor {

    override def receive = {
      case Register(jnlp, uuid) => registerJNLP(jnlp, uuid)
      case Sign    (jnlp, uuid) => signJNLP    (jnlp, uuid)
    }

    protected def registerJNLP(jnlp: JNLP, uuid: String): String = {
      val filenameBasis = genFilename(uuid)
      registerFile(jnlp.toXMLStr.getBytes, filenameBasis)
    }

    protected def signJNLP(jnlp: JNLP, uuid: String): String = {

      val baseJar    = new File(s"./public/deps/${jnlp.mainJar.jarName}")
      val targetPath = s"shims/$uuid/${baseJar.getName}"
      val targetJar  = new File("./public", targetPath)
      targetJar.getParentFile().mkdirs()

      val newJNLP = jnlp replaceMainJar targetPath

      val path = registerJNLP(newJNLP, uuid)

      JarSigner(baseJar, retrieveFile(genFilename(uuid)), targetJar, newJNLP.codebaseURI.toString, newJNLP.applicationName)

      path

    }

  }

}
