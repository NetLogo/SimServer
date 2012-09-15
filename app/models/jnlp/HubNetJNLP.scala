package models.jnlp

import java.net.URI

import scalaz.Validation

import models.web.ParamBox
import HubNetJNLPDefaults._
import HubNetJNLP.{ generateAppName, generateDesc, generateShortDesc }

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
                otherJars: Seq[Jar]                   = OtherJars,
                properties: Seq[Pair[String, String]] = Properties,
                arguments: Seq[String]                = Arguments
 ) extends NetLogoJNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, desc, shortDesc,
                       isOfflineAllowed, appNameInMenu, vendor, depsPath, otherJars, properties, arguments) {

    def this(codebaseURI: URI, jnlpLoc: String, mainClass: String, programName: String,
             roleStr: String, isOfflineAllowed: Boolean, otherJars: Seq[Jar],
             properties: Seq[Pair[String, String]], args: Seq[String])  {
      this(codebaseURI,
           jnlpLoc,
           MainJar,
           mainClass,
           generateAppName(programName, roleStr),
           generateDesc(programName, roleStr.toLowerCase),
           generateShortDesc(programName),
           isOfflineAllowed,
           AppNameInMenu,
           Vendor,
           DepsPath,
           otherJars,
           properties,
           args)
    }

}

object HubNetJNLP {

  private def generateArgs(key: String, value: String) = Seq(key, value)

  def generateAppName(programName: String, roleStr: String) = "%s HubNet %s".format(programName, roleStr)
  def generateDesc(programName: String, roleStr: String)    = "A HubNet %s for %s".format(roleStr, programName)
  def generateIPArgs(ip: String)                            = generateArgs("--ip", ip)
  def generateMainClass(isServer: Boolean)                  = if (isServer) HubNetJarManager.ServerMainClass else HubNetJarManager.ClientMainClass
  def generateModelURLArgs(url: String)                     = generateArgs("--url", url)
  def generatePortArgs(port: Int)                           = generateArgs("--port", port.toString)
  def generateShortDesc(programName: String)                = "HubNet (%s)".format(programName)
  def generateUserIDArgs(userID: String)                    = generateArgs("--id", userID)

  //@ Now that I have params for whether or not it's the server, what's the point of `roleStr`?
  // Basically, applies the default values into the boxes if they are are currently `NoneParam`s
  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], depsPathBox: ParamBox[String], otherJarsBox: ParamBox[Seq[(String, Boolean)]],
            propertiesBox: ParamBox[Seq[(String, String)]], argumentsBox: ParamBox[Seq[String]],
            programNameBox: ParamBox[String], roleStrBox: ParamBox[String], isServerBox: ParamBox[Boolean],
            modelURLBox: ParamBox[String], serverIPBox: ParamBox[String], serverPortBox: ParamBox[Int],
            userIDBox: ParamBox[String]) : Validation[String, JNLP] = {

    //@ Through proper use of applicatives, I should be able to abstract this over arity
    def contextify2IntoBox[T, U](f: T => T => U) = (box1: ParamBox[T]) => (box2: ParamBox[T]) => {
      for {
        a <- box1
        b <- box2
      } yield (f(a)(b))
    }

    val generateAppNameBox   = contextify2IntoBox((generateAppName _).curried)
    val generateDescBox      = contextify2IntoBox((generateDesc _).curried)

    val mainJar          = mainJarBox          orElseApply MainJar.jarName
    val mainClass        = mainClassBox        orElse      (isServerBox map generateMainClass)            orElseApply MainClass
    val applicationName  = applicationNameBox  orElse      generateAppNameBox(programNameBox)(roleStrBox) orElseApply ApplicationName
    val desc             = descBox             orElse      generateDescBox(programNameBox)(roleStrBox)    orElseApply Desc
    val shortDesc        = shortDescBox        orElse      (programNameBox map generateShortDesc)         orElseApply ShortDesc
    val isOfflineAllowed = isOfflineAllowedBox orElseApply IsOfflineAllowed
    val appNameInMenu    = appNameInMenuBox    orElseApply AppNameInMenu
    val vendor           = vendorBox           orElseApply Vendor
    val depsPath         = depsPathBox         orElseApply DepsPath
    val otherJars        = otherJarsBox        orElseApply Seq() map (_ ++ (MyJars map (jar => (jar.jarName, jar.isLazy))))
    val properties       = propertiesBox       orElseApply Properties
    val arguments        = argumentsBox        orElseApply Arguments map(_ ++ (modelURLBox   map generateModelURLArgs getOrElse Seq()) ++
                                                                              (serverIPBox   map generateIPArgs       getOrElse Seq()) ++
                                                                              (serverPortBox map generatePortArgs     getOrElse Seq()) ++
                                                                              (userIDBox     map generateUserIDArgs   getOrElse Seq()))

    NetLogoJNLP(
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

private[jnlp] object HubNetJNLPDefaults {
  private val Defs = NetLogoJNLPDefaults
  val MainJar                           = Defs.MainJar
  val MainClass                         = HubNetJarManager.ServerMainClass
  val ApplicationName                   = "HubNet WebStart"
  val Desc                              = "A HubNet WebStart app"
  val ShortDesc                         = "HubNet (WebStart)"
  val IsOfflineAllowed                  = Defs.IsOfflineAllowed
  val AppNameInMenu                     = "HubNet (WebStart)"
  val Vendor                            = Defs.Vendor
  val DepsPath                          = Defs.DepsPath
  val OtherJars:  Seq[Jar]              = Defs.OtherJars
  val MyJars:     Seq[Jar]              = Seq() // Yuck, need both this and `OtherJars` in order to avoid problems with constructor/API
  val Properties: Seq[(String, String)] = Defs.Properties
  val Arguments:  Seq[String]           = Defs.Arguments
}

object HubNetJarManager {
  val ServerMainClass = "org.nlogo.app.App"
  val ClientMainClass = "org.nlogo.hubnet.client.App"
}