package models.jnlp

import scalaz.ValidationNEL

import models.util.Util
import models.web.ParamBox

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/6/12
 * Time: 2:34 PM
 */

object NetLogoJNLP {

  import NetLogoJNLPDefaults._

  private def generateArgs(key: String, value: String) = Seq(key, value)
  def generateModelURLArgs(url: String)                = generateArgs("--url", url.replaceAll(" ", "+"))
  def generateLoggingArgs (isLogging: Boolean)         = Util.ifFirstWrapSecond(isLogging, "--logging").toSeq

  // Basically, applies the default values into the boxes if they are are currently `NoneParam`s
  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], depsPathBox: ParamBox[String], vmArgsBox: ParamBox[String],
            otherJarsBox: ParamBox[Seq[(String, Boolean)]], modelURLBox: ParamBox[String],
            propertiesBox: ParamBox[Seq[(String, String)]], argumentsBox: ParamBox[Seq[String]])
           (implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP] = {

    val codebaseURI      = codebaseURIBox      orElseApply thisServerCodebaseURL
    val mainJar          = mainJarBox          orElseApply MainJar.jarName
    val mainClass        = mainClassBox        orElseApply MainClass
    val applicationName  = applicationNameBox  orElseApply ApplicationName
    val desc             = descBox             orElseApply Desc
    val shortDesc        = shortDescBox        orElseApply ShortDesc
    val isOfflineAllowed = isOfflineAllowedBox orElseApply IsOfflineAllowed
    val appNameInMenu    = appNameInMenuBox    orElseApply AppNameInMenu
    val vendor           = vendorBox           orElseApply Vendor
    val depsPath         = depsPathBox         orElseApply DepsPath
    val vmArgs           = vmArgsBox           orElseApply VMArgs
    val otherJars        = otherJarsBox        orElseApply Seq() map (_ ++ ((NeededJars ++ OtherJars) map (jar => (jar.jarName, jar.isLazy))))
    val properties       = propertiesBox       orElseApply Properties
    val arguments        = argumentsBox        orElseApply Arguments map(_ ++ (modelURLBox map generateModelURLArgs getOrElse Seq()))

    JNLP(
      codebaseURI,
      jnlpLocBox,
      mainJar,
      mainClass,
      applicationName,
      desc,
      shortDesc,
      isOfflineAllowed,
      appNameInMenu,
      vendor,
      depsPath,
      vmArgs,
      otherJars,
      properties,
      arguments
    )

  }

}

private[jnlp] object NetLogoJNLPDefaults {
  import models.util.Util.noneIfEmpty
  private val Defs                      = JNLPDefaults
  val MainJar                           = new MainJar("NetLogo.jar")
  val MainClass                         = "org.nlogo.app.App"
  val ApplicationName                   = "NetLogo"
  val Desc                              = "A NetLogo WebStart app"
  val ShortDesc                         = "NetLogo (WebStart)"
  val IsOfflineAllowed                  = Defs.IsOfflineAllowed
  val AppNameInMenu                     = "NetLogo (WebStart)"
  val Vendor                            = "CCL"
  val DepsPath                          = "deps"
  val VMArgs                            = noneIfEmpty(Defs.VMArgs) map (_ + " ") getOrElse "" // Can't set default memory args; causes Mountain Lion failure
  val OtherJars:  Seq[Jar]              = Defs.OtherJars
  val NeededJars: Seq[Jar]              = NetLogoJarManager.getDefaultJars
  val Properties: Seq[(String, String)] = Defs.Properties
  val Arguments:  Seq[String]           = Defs.Arguments
}

private object NetLogoJarManager {

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
    "knockoff_2.9.2-0.8.1.jar",
    "picocontainer-2.13.6.jar",
    "quaqua-7.3.4.jar",
    "scala-library.jar",
    "swing-layout-7.3.4.jar"
  )

  def getDefaultJars : Seq[Jar] = {
    def generateJar(name: String, isLazy: Boolean) = new Jar(name, isLazy)
    val jarsAndIsLazyPairs = Seq((DefaultLazyJarNames, true), (DefaultJarNames, false))
    jarsAndIsLazyPairs map { case (jars, isLazy) => jars map (generateJar(_, isLazy)) } flatten
  }

}

