package models.jnlp

import
  java.io.File

import
  scala.concurrent.duration._

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

  // Don't sign any applications external to us! --JAB (11/12/13)
  def registerAndSign(jnlp: JNLP, uuid: String, defaultCodebase: String): String =
    if (defaultCodebase == jnlp.codebaseURI.toString)
      signJNLP(jnlp, uuid)
    else
      registerJNLP(jnlp, uuid)

  def genFilename(basis: String): String =
    s"$MyFolderName/${basis}.jnlp"

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
