package models.jnlp

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/13/12
 * Time: 4:08 PM
 */

object JarManager {

  private val DefaultLazyJarNames = Seq(
    "extensions.jar",
    "commons-codec-1.6.jar",
    "commons-logging-1.1.1.jar",
    "httpclient-4.2.jar",
    "httpcore-4.2.jar"
  )

  private val DefaultJarNames = Seq(
    "asm-all-3.3.1.jar",
    "gluegen-rt-1.1.1.jar",
    "jhotdraw-6.0b1.jar",
    "jmf-2.1.1e.jar",
    "jogl-1.1.1.jar",
    "log4j-1.2.16.jar",
    "mrjadapter-1.2.jar",
    "picocontainer-2.13.6.jar",
    "quaqua-7.3.4.jar",
    "scala-library.jar",
    "swing-layout-7.3.4.jar"
  )
  
  def getDefaultJars : Seq[Jar] = {
    def generateJar(name: String, isLazy: Boolean) = new Jar(name, isLazy)
    val listLazyPairs = Seq((DefaultLazyJarNames, true), (DefaultJarNames, false))
    listLazyPairs map { case (list, isLazy) => list map (generateJar(_, isLazy)) } flatten
  }
}
