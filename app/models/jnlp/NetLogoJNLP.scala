package models.jnlp

import java.net.URI

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
                appTitle: String                      = AppTitle,
                desc: String                          = Desc,
                shortDesc: String                     = ShortDesc,
                isOfflineAllowed: Boolean             = IsOfflineAllowed,
                appNameInMenu: String                 = AppNameInMenu,
                vendor: String                        = Vendor,
                depsPath: String                      = DepsPath,
                otherJars: Seq[Jar]                   = Seq(),
                properties: Seq[Pair[String, String]] = Seq(),
                arguments: Seq[String]                = Seq()
 ) extends JNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, appTitle, desc, shortDesc,
                isOfflineAllowed, appNameInMenu, vendor, depsPath, otherJars, properties, arguments)

private[jnlp] object NetLogoJNLPDefaults {
  val MainJar          = new MainJar("NetLogo.jar")
  val MainClass        = "org.nlogo.app.App"
  val ApplicationName  = "NetLogo WebStart"
  val AppTitle         = "NetLogo"
  val Desc             = "A NetLogo WebStart app"
  val ShortDesc        = "NetLogo (WebStart)"
  val IsOfflineAllowed = JNLPDefaults.IsOfflineAllowed
  val AppNameInMenu    = "NetLogo (WebStart)"
  val Vendor           = "CCL"
  val DepsPath         = "misc/deps"
}
