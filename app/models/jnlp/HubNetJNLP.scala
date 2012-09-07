package models.jnlp

import java.net.URI

import HubNetJNLPDefaults._

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/6/12
 * Time: 2:59 PM
 */

class HubNetJNLP(
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
                otherJars: Seq[Jar]                   = Seq(),
                properties: Seq[Pair[String, String]] = Seq(),
                arguments: Seq[String]                = Seq()
 ) extends NetLogoJNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, desc, shortDesc,
                       isOfflineAllowed, appNameInMenu, vendor, depsPath, otherJars, properties, arguments) {

    def this(codebaseURI: URI, jnlpLoc: String, mainClass: String, programName: String,
             clientOrServerStr: String, isOfflineAllowed: Boolean, otherJars: Seq[Jar],
             properties: Seq[Pair[String, String]], args: Seq[String])  {
        this(codebaseURI,
             jnlpLoc,
             MainJar,
             mainClass,
             "%s HubNet %s".format(programName, clientOrServerStr),                   // applicationName
             "A HubNet %s for %s".format(clientOrServerStr.toLowerCase, programName), // desc
             "HubNet (%s)".format(programName),                                       // shortDesc
             isOfflineAllowed,
             AppNameInMenu,
             Vendor,
             DepsPath,
             otherJars,
             properties,
             args)
    }

}

private[jnlp] object HubNetJNLPDefaults {
  private val Defs = NetLogoJNLPDefaults
  val MainJar          = Defs.MainJar
  val MainClass        = "org.nlogo.hubnet.client.App"
  val ApplicationName  = "HubNet WebStart"
  val Desc             = "A HubNet WebStart app"
  val ShortDesc        = "HubNet (WebStart)"
  val IsOfflineAllowed = Defs.IsOfflineAllowed
  val AppNameInMenu    = "HubNet (WebStart)"
  val Vendor           = Defs.Vendor
  val DepsPath         = Defs.DepsPath
}
