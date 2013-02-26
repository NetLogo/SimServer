package models.jnlp

import java.net.URI

import scalaz._, Scalaz._

import JNLPDefaults._
import models.web.ParamBox

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 3:40 PM
 */

class JNLP(
 /* Required */ codebaseURI: URI,   // Should just be the 'public' folder
 /* Required */ jnlpLoc: String,
 /* Required */ mainJar: MainJar,
 /* Required */ mainClass: String,
                applicationName: String               = ApplicationName,
                desc: String                          = Desc,
                shortDesc: String                     = ShortDesc,
                isOfflineAllowed: Boolean             = IsOfflineAllowed,
                appNameInMenu: String                 = AppNameInMenu, // Used if we ever want to save application shortcuts
                vendor: String                        = Vendor,
                packEnabled: Boolean                  = PackEnabled,
                depsPath: String                      = DepsPath,
                vmArgs: String                        = VMArgs,
                otherJars: Seq[Jar]                   = OtherJars,
                properties: Seq[Pair[String, String]] = Properties,
                arguments: Seq[String]                = Arguments
 ) {

  def toXMLStr : String = {

    val jars = mainJar +: otherJars

    val offlineAllowedStr = if (isOfflineAllowed) "\n" + formatXMLNode("offline-allowed", Map(), 2) else ""
    val propsStr = properties map { case (key, value) => formatXMLNode("property", Map("name" -> key, "value" -> value), 2) } mkString("\n", "\n", "")
    val argsStr = arguments map (formatXMLPair("argument", Map(), _, 2)) mkString("\n", "\n", "\n" + formatIndentation(1))
    val appDesc = "\n" + formatXMLPair("application-desc", Map("name" -> applicationName, "main-class" -> mainClass), argsStr, 1)
    val jarsStr = jars map {
      jar =>
        import jar._
        val hrefProps =
          Seq("href" -> s"$depsPath/$jarName") ++
            (if (isMain) Seq("main"     -> "true") else Seq()) ++
            (if (isLazy) Seq("download" -> "lazy") else Seq())
        formatXMLNode("jar", Map(hrefProps: _*), 2)
    } mkString ("\n", "\n", "")

    generateJNLPString(codebaseURI.toString, jnlpLoc, applicationName, vendor, packEnabled.toString,
                       desc, shortDesc, offlineAllowedStr, vmArgs, jarsStr, propsStr, appDesc)

  }

  private def formatXMLNode(tagName: String, attrKVs: Map[String, String], indentationLevel: Int) : String =
    s"${formatIndentation(indentationLevel)}<${tagName}${formatAttrs(attrKVs)} />"

  private def formatXMLPair(tagName: String, attrKVs: Map[String, String], data: String, indentationLevel: Int) : String =
    s"${formatIndentation(indentationLevel)}<${tagName}${formatAttrs(attrKVs)}>$data</$tagName>"

  private def formatAttrs(attrKVs: Map[String, String]) : String = {
    val attrs = attrKVs.toList map { case (k, v) => s"""$k=\"$v\" """.trim } mkString " "
    if (attrs.isEmpty) "" else " " + attrs
  }

  private def formatIndentation(level: Int) = (Stream continually " " take (level * 4)).mkString
  
}

object JNLP {

  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], packEnabledBox: ParamBox[Boolean], depsPathBox: ParamBox[String],
            vmArgsBox: ParamBox[String], otherJarsBox: ParamBox[Seq[(String, Boolean)]], propertiesBox: ParamBox[Seq[(String, String)]],
            argumentsBox: ParamBox[Seq[String]]) : ValidationNEL[String, JNLP] = {

    val errorStrFormat  = "Bad data supplied for: " + (_: String)
    val f               =  (box: ParamBox[String]) => box map (_.successNel[String]) getOrElse errorStrFormat(box.key).failNel[String]

    (f(codebaseURIBox) |@| f(jnlpLocBox) |@| f(mainJarBox) |@| f(mainClassBox)) {
      (codebaseURIStr, jnlpLocStr, mainJarStr, mainClassStr) =>

        def jar(pathAndIsLazy: (String, Boolean)) = pathAndIsLazy match { case (path, isLazy) => new Jar(path, isLazy) }

        val codebaseURI      = new URI(codebaseURIStr)
        val jnlpLoc          = jnlpLocStr
        val mainJar          = new MainJar(mainJarStr)
        val mainClass        = mainClassStr

        val applicationName  = applicationNameBox           getOrElse ApplicationName
        val desc             = descBox                      getOrElse Desc
        val shortDesc        = shortDescBox                 getOrElse ShortDesc
        val isOfflineAllowed = isOfflineAllowedBox          getOrElse IsOfflineAllowed
        val appNameInMenu    = appNameInMenuBox             getOrElse AppNameInMenu
        val vendor           = vendorBox                    getOrElse Vendor
        val packEnabled      = packEnabledBox               getOrElse PackEnabled
        val depsPath         = depsPathBox                  getOrElse DepsPath
        val vmArgs           = vmArgsBox                    getOrElse VMArgs
        val otherJars        = otherJarsBox map (_ map jar) getOrElse OtherJars
        val properties       = propertiesBox                getOrElse Properties
        val arguments        = argumentsBox                 getOrElse Arguments

        new JNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, desc, shortDesc, isOfflineAllowed,
                 appNameInMenu, vendor, packEnabled, depsPath, vmArgs, otherJars, properties, arguments)
    }
  }
}

private[jnlp] object JNLPDefaults {

  val ApplicationName                   = "Unnamed WebStart Application"
  val Desc                              = "A Java WebStart app"
  val ShortDesc                         = "Java WebStart"
  val IsOfflineAllowed                  = true
  val AppNameInMenu                     = "Java WebStart"
  val Vendor                            = "[Unknown Vendor]"
  val DepsPath                          = "deps"
  val PackEnabled                       = true
  val VMArgs                            = ""
  val OtherJars:  Seq[Jar]              = Seq()
  val Properties: Seq[(String, String)] = Seq()
  val Arguments:  Seq[String]           = Seq()

  // Also, Guns N' Roses once wrote a song about the following code; it was called "Welcome to the Jungle"
  def generateJNLPString(codebaseURI: String, jnlpLoc: String, applicationName: String, vendor: String,
                         packEnabledStr: String, desc: String, shortDesc: String, offlineAllowedStr: String,
                         vmArgs: String, jarsStr: String, propsStr: String, appDesc: String) =
   s"""
      |<?xml version="1.0" encoding="UTF-8"?>
      |<jnlp spec="1.0+" codebase="${codebaseURI.toString}" href="$jnlpLoc">
      |    <information>
      |        <title>$applicationName</title>
      |        <vendor>$vendor</vendor>
      |        <description>$desc</description>
      |        <description kind="short">$shortDesc</description>$offlineAllowedStr
      |    </information>
      |    <security> <all-permissions/> </security>
      |    <resources>
      |
      |        <property name="jnlp.packEnabled" value="$packEnabledStr"/>
      |
      |        <!-- Application Resources -->
      |        <j2se version="1.6+ 1.7+" java-vm-args="$vmArgs" href="http://java.sun.com/products/autodl/j2se"/>$jarsStr
      |
      |        <!-- System Properties -->$propsStr
      |
      |    </resources>$appDesc
      |    <update check="timeout" policy="always"/>
      |</jnlp>
    """.stripMargin.trim

}
