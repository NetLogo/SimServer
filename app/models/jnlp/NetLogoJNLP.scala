package models.jnlp

import java.net.URI

import scalaz.Validation

import models.web.ParamBox
import NetLogoJNLPDefaults._

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/6/12
 * Time: 2:34 PM
 */

class NetLogoJNLP(
 /* Required */ codebaseURI: URI,
 /* Required */ jnlpLoc: String,
                mainJar: MainJar                      = MainJar,
                mainClass: String                     = MainClass,
                applicationName: String               = ApplicationName,
                desc: String                          = Desc,
                shortDesc: String                     = ShortDesc,
                isOfflineAllowed: Boolean             = IsOfflineAllowed,
                appNameInMenu: String                 = AppNameInMenu,
                vendor: String                        = Vendor,
                depsPath: String                      = DepsPath,
                otherJars: Seq[Jar]                   = OtherJars,
                properties: Seq[Pair[String, String]] = Properties,
                arguments: Seq[String]                = Arguments
 ) extends JNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, desc, shortDesc,
                isOfflineAllowed, appNameInMenu, vendor, depsPath, otherJars, properties, arguments)

object NetLogoJNLP {

  // Basically, applies the default values into the boxes if they are are currently `NoneParam`s
  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], depsPathBox: ParamBox[String], otherJarsBox: ParamBox[Seq[(String, Boolean)]],
            propertiesBox: ParamBox[Seq[(String, String)]], argumentsBox: ParamBox[Seq[String]]) : Validation[String, JNLP] = {

    val mainJar          = mainJarBox          orElseApply MainJar.jarName
    val mainClass        = mainClassBox        orElseApply MainClass
    val applicationName  = applicationNameBox  orElseApply ApplicationName
    val desc             = descBox             orElseApply Desc
    val shortDesc        = shortDescBox        orElseApply ShortDesc
    val isOfflineAllowed = isOfflineAllowedBox orElseApply IsOfflineAllowed
    val appNameInMenu    = appNameInMenuBox    orElseApply AppNameInMenu
    val vendor           = vendorBox           orElseApply Vendor
    val depsPath         = depsPathBox         orElseApply DepsPath
    val otherJars        = otherJarsBox        orElseApply Seq() map (_ ++ (OtherJars map (jar => (jar.jarName, jar.isLazy))))
    val properties       = propertiesBox       orElseApply Properties
    val arguments        = argumentsBox        orElseApply Arguments

    JNLP(
      codebaseURIBox,
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
      otherJars,
      properties,
      arguments
    )

  }

}

private[jnlp] object NetLogoJNLPDefaults {
  private val Defs                      = JNLPDefaults
  val MainJar                           = new MainJar("NetLogo.jar")
  val MainClass                         = "org.nlogo.app.App"
  val ApplicationName                   = "NetLogo"
  val Desc                              = "A NetLogo WebStart app"
  val ShortDesc                         = "NetLogo (WebStart)"
  val IsOfflineAllowed                  = Defs.IsOfflineAllowed
  val AppNameInMenu                     = "NetLogo (WebStart)"
  val Vendor                            = "CCL"
  val DepsPath                          = "misc/deps"
  val OtherJars:  Seq[Jar]              = NetLogoJarManager.getDefaultJars ++: Defs.OtherJars
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

