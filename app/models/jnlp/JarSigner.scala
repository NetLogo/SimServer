package models.jnlp

import
  java.{ io => jio, nio },
    jio.{ File, FileOutputStream },
    nio.file.Files

import
  scala.{ io, sys },
    io.{ BufferedSource, Source },
    sys.process.Process

object JarSigner {

  def apply(baseJar: File, jnlpFile: File, targetFile: File, codebase: String, appName: String): Unit = {

    implicit val tempJarDir = Files.createTempDirectory(null).toFile

    extractJar(baseJar.getAbsolutePath)
    insertJNLP(jnlpFile)

    val manifestFile = createManifest(codebase, appName)
    val jarFile      = rebundleJar(targetFile, baseJar.getName, manifestFile.getAbsolutePath)

    signAndPack(jarFile)

  }

  private def extractJar(jarPath: String)(implicit temp: File): Unit =
    shell(s"jar -xf $jarPath")

  private def insertJNLP(jnlpFile: File)(implicit temp: File): Unit = {

    val jnlpInfDir = new File(temp, "JNLP-INF")
    jnlpInfDir.mkdirs()

    val newJnlpFile = new File(jnlpInfDir, "APPLICATION.JNLP")
    val jnlpText    = usingSource(_.fromFile(jnlpFile))(_.mkString)

    using(new FileOutputStream(newJnlpFile))(_.write(jnlpText.getBytes))

  }

  private def rebundleJar(target: File, baseJarName: String, manifestPath: String)(implicit temp: File): File = {

    val filesStr = temp.listFiles().toSeq map (_.getAbsolutePath drop (temp.getAbsolutePath.length + 1)) filter (_ != "META-INF") mkString " "
    target.getParentFile.mkdirs()

    shell(s"jar -cfm ${target.getAbsolutePath} $manifestPath $filesStr")

    target

  }

  private def createManifest(codebase: String, appName: String): File = {

    val manifestFile = Files.createTempFile(null, null).toFile
    using(new FileOutputStream(manifestFile)) {
      _.write(genManifestText(codebase, appName).getBytes)
    }

    manifestFile

  }

  private def genManifestText(codebase: String, appName: String): String =
   s"""
      |Permissions: all-permissions
      |Codebase: $codebase
      |Application-Name: $appName
    """.trim.stripMargin

  private def signAndPack(jarFile: File): Unit = {

    val jarPath  = jarFile.getAbsolutePath
    val commands = Seq(
      s"pack200 -r $jarPath",
      s"jarsigner -storepass ${SigningConfig.KeystorePass} -keypass ${SigningConfig.KeyPass} $jarPath ${SigningConfig.KeyName}",
      s"pack200 --modification-time=latest --effort=9 --no-keep-file-order ${jarPath}.pack.gz $jarPath"
    )

    commands foreach shell

  }

  private def using[T <: { def close() }, U](closeable: T)(f: (T) => U): U =
    try f(closeable) finally closeable.close()

  private def usingSource[T](sourceGenFunc: (Source.type) => BufferedSource)(f: (BufferedSource) => T): T =
    using(sourceGenFunc(Source))(f)

  private def shell(cmd: String)(implicit context: File = new File(System.getProperty("user.home"))): String =
    Process(cmd, context).!!

}
