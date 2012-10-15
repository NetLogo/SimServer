package models.jnlp

import java.net.URI

import scalaz.{ Failure, Success, Validation }

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
          Seq("href" -> "%s/%s".format(depsPath, jarName)) ++
            (if (isMain) Seq("main"     -> "true") else Seq()) ++
            (if (isLazy) Seq("download" -> "lazy") else Seq())
        formatXMLNode("jar", Map(hrefProps: _*), 2)
    } mkString ("\n", "\n", "")

    generateJNLPString(codebaseURI.toString, jnlpLoc, applicationName, vendor, desc,
                       shortDesc, offlineAllowedStr, vmArgs, jarsStr, propsStr, appDesc)

  }

  private def formatXMLNode(tagName: String, attrKVs: Map[String, String], indentationLevel: Int) : String = {
    "%s<%s %s/>".format(formatIndentation(indentationLevel), tagName, formatAttrs(attrKVs))
  }

  private def formatXMLPair(tagName: String, attrKVs: Map[String, String], data: String, indentationLevel: Int) : String = {
    "%s<%s%s>%s</%s>".format(formatIndentation(indentationLevel), tagName,
                             { val attrs = formatAttrs(attrKVs); if (attrs.isEmpty) "" else " " + attrs }, data, tagName)
  }

  private def formatAttrs(attrKVs: Map[String, String]) : String = {
    attrKVs.toList map { case (key, value) => "%s=\"%s\"".format(key, value) } mkString " "
  }

  private def formatIndentation(level: Int) = (Stream continually " " take (level * 4)).mkString
  
}

object JNLP {

  def apply(codebaseURIBox: ParamBox[String], jnlpLocBox: ParamBox[String], mainJarBox: ParamBox[String],
            mainClassBox: ParamBox[String], applicationNameBox: ParamBox[String], descBox: ParamBox[String],
            shortDescBox: ParamBox[String], isOfflineAllowedBox: ParamBox[Boolean], appNameInMenuBox: ParamBox[String],
            vendorBox: ParamBox[String], depsPathBox: ParamBox[String], vmArgsBox: ParamBox[String],
            otherJarsBox: ParamBox[Seq[(String, Boolean)]], propertiesBox: ParamBox[Seq[(String, String)]],
            argumentsBox: ParamBox[Seq[String]]) : Validation[String, JNLP] = {

    type VSList = Validation[String, List[String]]
    def append(v1: VSList, v2: VSList) : VSList = (v1, v2) match {
      case (Success(s1), Success(s2)) => Success(s1 ++ s2)
      case (Success(s),  Failure(f))  => Failure(f)
      case (Failure(f),  Success(s))  => Failure(f)
      case (Failure(f1), Failure(f2)) => Failure(f1 + "\n" + f2)
    }

    val requireds = List(codebaseURIBox, jnlpLocBox, mainJarBox, mainClassBox)

    val errorStrFormat  = "Bad data supplied for: " + (_: String)
    val rootValidations = requireds map (box => box map (x => Success(List(x))) getOrElse Failure(errorStrFormat(box.key)))
    val validation      = rootValidations reduce append

    validation flatMap {

      case codebaseURIStr :: jnlpLocStr :: mainJarStr :: mainClassStr :: Nil =>

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
        val depsPath         = depsPathBox                  getOrElse DepsPath
        val vmArgs           = vmArgsBox                    getOrElse VMArgs
        val otherJars        = otherJarsBox map (_ map jar) getOrElse OtherJars
        val properties       = propertiesBox                getOrElse Properties
        val arguments        = argumentsBox                 getOrElse Arguments

        Success(
          new JNLP(codebaseURI, jnlpLoc, mainJar, mainClass, applicationName, desc, shortDesc,
                   isOfflineAllowed, appNameInMenu, vendor, depsPath, vmArgs, otherJars, properties, arguments)
        )

      case _ =>
        Failure("Somehow, an invalid set of required JNLP parameters was passed in/matched on.")

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
  val DepsPath                          = "misc/deps"
  val VMArgs                            = ""
  val OtherJars:  Seq[Jar]              = Seq()
  val Properties: Seq[(String, String)] = Seq()
  val Arguments:  Seq[String]           = Seq()

  // It's tempting to use `String.format` here, but I fear that it would get far too confusing
  // (Also, Guns N' Roses once wrote a song about the following code; it was called "Welcome to the Jungle")
  def generateJNLPString(codebaseURI: String, jnlpLoc: String, applicationName: String,
                         vendor: String, desc: String, shortDesc: String, offlineAllowedStr: String,
                         vmArgs: String, jarsStr: String, propsStr: String, appDesc: String) = (
//@ String interpolation, plox!  (Scala 2.10, I'm waiting here for youuuuuuuuu!  And for Play for follow suuuuuuuuuit!)
"""
<?xml version="1.0" encoding="UTF-8"?>
<jnlp spec="1.0+" codebase=""" + '"' + codebaseURI.toString + '"' + """ href=""" + '"' + jnlpLoc + '"' + """>
    <information>
        <title>""" + applicationName + """</title>
        <vendor>""" + vendor + """</vendor>
        <description>""" + desc + """</description>
        <description kind="short">""" + shortDesc + """</description>""" + offlineAllowedStr + """
    </information>
    <security> <all-permissions/> </security>
    <resources>

        <!-- Application Resources -->
        <j2se version="1.5+ 1.6+ 1.7+" java-vm-args=""" + '"' + vmArgs + '"' + """ href="http://java.sun.com/products/autodl/j2se"/>""" +
        jarsStr + """

        <!-- System Properties -->""" +
        propsStr + """

    </resources>""" +
    appDesc + """
    <update check="timeout" policy="always"/>
</jnlp>
""").trim

}
