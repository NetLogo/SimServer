package models.jnlp

import scalaz.ValidationNEL

import models.web.ParamBox

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/6/12
 * Time: 2:59 PM
 */

object HubNetJNLP {

  import HubNetJarManager._, HubNetJNLPDefaults._

  private def generateArgs(key: String, value: String) = Seq(key, value)

  def generateAppName(programName: String, roleStr: String) = "%s HubNet %s".format(programName, roleStr)
  def generateDesc(programName: String, roleStr: String)    = "A HubNet %s for %s".format(roleStr, programName)
  def generateIPArgs(ip: String)                            = generateArgs("--ip", ip)
  def generatePortArgs(port: Int)                           = generateArgs("--port", port.toString)
  def generateShortDesc(programName: String)                = "HubNet (%s)".format(programName)
  def generateUserIDArgs(userID: String)                    = generateArgs("--id", userID)

  // Basically, applies the default values into the boxes if they are are currently `NoneParam`s
  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], depsPathBox: ParamBox[String], vmArgsBox: ParamBox[String],
            otherJarsBox: ParamBox[Seq[(String, Boolean)]], propertiesBox: ParamBox[Seq[(String, String)]],
            argumentsBox: ParamBox[Seq[String]], programNameBox: ParamBox[String])
           (roleStrBox: ParamBox[String], isServerBox: ParamBox[Boolean], modelURLBox: ParamBox[String],
            serverIPBox: ParamBox[String], serverPortBox: ParamBox[Int],  userIDBox: ParamBox[String])
           (implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP] = {

    // Through proper use of applicatives, I would be able to abstract this over arity.
    // But I won't, because that'd be a lot of work. --JAB (11/20/12)
    def contextify2IntoBox[T, U](f: T => T => U) = (box1: ParamBox[T]) => (box2: ParamBox[T]) => {
      for {
        a <- box1
        b <- box2
      } yield (f(a)(b))
    }

    val generateAppNameBox   = contextify2IntoBox((generateAppName _).curried)
    val generateDescBox      = contextify2IntoBox((generateDesc _).curried)

    // Wow, `flatMap` sucks on this thing...
    val isServerPlus = isServerBox flatMap (x => ParamBox(isServerBox.key, if (x) Option(x) else None))

    val (mainClassStr, vmArgsStr, neededsSeq, othersSeq) = isServerPlus map (
      _ => (ServerMainClass, ServerVMArgs, NeededJars, OtherJars)
    ) getOrElse (ClientMainClass, ClientVMArgs, ClientNeededJars, ClientOtherJars)

    val mainJar          = mainJarBox          orElseApply MainJar.jarName
    val mainClass        = mainClassBox        orElseApply mainClassStr
    val applicationName  = applicationNameBox  orElse      generateAppNameBox(programNameBox)(roleStrBox) orElseApply ApplicationName
    val desc             = descBox             orElse      generateDescBox(programNameBox)(roleStrBox)    orElseApply Desc
    val shortDesc        = shortDescBox        orElse      (programNameBox map generateShortDesc)         orElseApply ShortDesc
    val isOfflineAllowed = isOfflineAllowedBox orElseApply IsOfflineAllowed
    val appNameInMenu    = appNameInMenuBox    orElseApply AppNameInMenu
    val vendor           = vendorBox           orElseApply Vendor
    val depsPath         = depsPathBox         orElseApply DepsPath
    val vmArgs           = vmArgsBox           orElseApply vmArgsStr
    val otherJars        = otherJarsBox        orElseApply Seq() map (_ ++ ((neededsSeq ++ othersSeq) map (jar => (jar.jarName, jar.isLazy))))
    val properties       = propertiesBox       orElseApply Properties
    val arguments        = argumentsBox        orElseApply Arguments map(_ ++ (serverIPBox   map generateIPArgs       getOrElse Seq()) ++
                                                                              (serverPortBox map generatePortArgs     getOrElse Seq()) ++
                                                                              (userIDBox     map generateUserIDArgs   getOrElse Seq()))

    if (isServerPlus is true)
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
        vmArgs,
        otherJars,
        modelURLBox, // For `NetLogoJNLP` only
        properties,
        arguments
      )
    else
      JNLP(
        codebaseURIBox orElseApply thisServerCodebaseURL,
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
  val VMArgs                            = ""
  val OtherJars:  Seq[Jar]              = Seq()
  val NeededJars: Seq[Jar]              = Seq()
  val Properties: Seq[(String, String)] = Defs.Properties
  val Arguments:  Seq[String]           = Defs.Arguments

  val ClientNeededJars = Seq(new Jar("scala-library.jar", false), new Jar("mrjadapter-1.2.jar", false))
  val ClientOtherJars  = Seq()

}

object HubNetJarManager {
  val ServerMainClass = "org.nlogo.app.App"
  val ClientMainClass = "org.nlogo.hubnet.client.App"
  val ServerVMArgs    = NetLogoJNLPDefaults.VMArgs
  val ClientVMArgs    = ""
}
