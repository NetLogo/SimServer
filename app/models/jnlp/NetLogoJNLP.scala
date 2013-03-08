package models.jnlp

import
  scalaz.ValidationNel

import
  models.{ util, web },
    util.Util,
    web.ParamBox

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
            vendorBox: ParamBox[String], packEnabledBox: ParamBox[Boolean], depsPathBox: ParamBox[String],
            vmArgsBox: ParamBox[String], otherJarsBox: ParamBox[Seq[(String, Boolean)]], modelURLBox: ParamBox[String],
            usesExtensionsBox: ParamBox[Boolean], propertiesBox: ParamBox[Seq[(String, String)]],
            argumentsBox: ParamBox[Seq[String]])
           (implicit thisServerCodebaseURL: String) : ValidationNel[String, JNLP] = {

    val extensionsJar    = usesExtensionsBox map (if (_) Option(ExtensionsJar) else None) getOrElse Option(ExtensionsJar)

    val codebaseURI      = codebaseURIBox      orElseApply thisServerCodebaseURL
    val mainJar          = mainJarBox          orElseApply MainJar.jarName
    val mainClass        = mainClassBox        orElseApply MainClass
    val applicationName  = applicationNameBox  orElseApply ApplicationName
    val desc             = descBox             orElseApply Desc
    val shortDesc        = shortDescBox        orElseApply ShortDesc
    val isOfflineAllowed = isOfflineAllowedBox orElseApply IsOfflineAllowed
    val appNameInMenu    = appNameInMenuBox    orElseApply AppNameInMenu
    val vendor           = vendorBox           orElseApply Vendor
    val packEnabled      = packEnabledBox      orElseApply PackEnabled
    val depsPath         = depsPathBox         orElseApply DepsPath
    val vmArgs           = vmArgsBox           orElseApply VMArgs
    val otherJars        = otherJarsBox        orElseApply Seq() map (_ ++ ((extensionsJar ++ NeededJars ++ OtherJars) map (jar => (jar.jarName, jar.isLazy))))
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
      packEnabled,
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
  val MainJar                           = new MainJar("NetLogoGUI.jar")
  val MainClass                         = "org.nlogo.app.App"
  val ApplicationName                   = "NetLogo"
  val Desc                              = "A NetLogo WebStart app"
  val ShortDesc                         = "NetLogo (WebStart)"
  val IsOfflineAllowed                  = Defs.IsOfflineAllowed
  val AppNameInMenu                     = "NetLogo (WebStart)"
  val Vendor                            = "CCL"
  val PackEnabled                       = Defs.PackEnabled
  val DepsPath                          = "deps"
  val VMArgs                            = noneIfEmpty(Defs.VMArgs) map (_ + " ") getOrElse "" // Can't set default memory args; causes Mountain Lion failure
  val OtherJars:  Seq[Jar]              = Defs.OtherJars
  val NeededJars: Seq[Jar]              = NetLogoJarManager.getDefaultJars
  val ExtensionsJar: Jar                = NetLogoJarManager.ExtensionsJar
  val Properties: Seq[(String, String)] = Defs.Properties
  val Arguments:  Seq[String]           = Defs.Arguments
}

private object NetLogoJarManager {

  private val DefaultLazyJarNames = Seq(
    "commons-codec-1.6.jar",
    "commons-logging-1.1.1.jar",
    "httpclient-4.2.jar",
    "httpcore-4.2.jar"
  )

  private val DefaultJarNames = Seq(
    "akka-actor_2.10-2.1.0.jar",
    "asm-all-3.3.1.jar",
    "config-1.0.0.jar",
    "gluegen-rt-1.1.1.jar",
    "jhotdraw-6.0b1.jar",
    "jmf-2.1.1e.jar",
    "jogl-1.1.1.jar",
    "logging.jar",
    "log4j-1.2.16.jar",
    "mrjadapter-1.2.jar",
    "NetLogoEngine.jar",
    "knockoff_2.10-0.8.1.jar",
    "picocontainer-2.13.6.jar",
    "quaqua-7.3.4.jar",
    "scala-library.jar",
    "swing-layout-7.3.4.jar"
  )

  val ExtensionsJar = new Jar("extensions.jar", true)

  def getDefaultJars : Seq[Jar] = {
    def generateJar(name: String, isLazy: Boolean) = new Jar(name, isLazy)
    val jarsAndIsLazyPairs = Seq((DefaultLazyJarNames, true), (DefaultJarNames, false))
    jarsAndIsLazyPairs map { case (jars, isLazy) => jars map (generateJar(_, isLazy)) } flatten
  }

}

