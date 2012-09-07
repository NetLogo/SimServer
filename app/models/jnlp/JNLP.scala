package models.jnlp

import java.net.URI

import JNLPDefaults._

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
                otherJars: Seq[Jar]                   = Seq(),
                properties: Seq[Pair[String, String]] = Seq(),
                arguments: Seq[String]                = Seq()
 ) {

  def toXMLStr : String = {

    val jars = mainJar +: NetLogoJarManager.getDefaultJars ++: otherJars
    val props = properties // No additions as of right now
    val args = arguments   // Currently, no additions

    val offlineAllowedStr = if (isOfflineAllowed) "\n" + formatXMLNode("offline-allowed", Map(), 2) else ""
    val propsStr = props map { case (key, value) => formatXMLNode("property", Map("name" -> key, "value" -> value), 2) } mkString("\n", "\n", "")
    val argsStr = args map (formatXMLPair("argument", Map(), _, 2)) mkString("\n", "\n", "\n" + formatIndentation(1))
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

    generateJNLPString(codebaseURI.toString, jnlpLoc, applicationName, vendor, desc, shortDesc, offlineAllowedStr, jarsStr, propsStr, appDesc)

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

private[jnlp] object JNLPDefaults {

  val ApplicationName  = "Unnamed WebStart Application"
  val Desc             = "A NetLogo WebStart app"
  val ShortDesc        = "NetLogo (WebStart)"
  val IsOfflineAllowed = true
  val AppNameInMenu    = "NetLogo (WebStart)"
  val Vendor           = "CCL"
  val DepsPath         = "misc/deps"

  // It's tempting to use `String.format` here, but I fear that it would get far too confusing
  // (Also, Guns N' Roses once wrote a song about the following code; it was called "Welcome to the Jungle")
  def generateJNLPString(codebaseURI: String, jnlpLoc: String, applicationName: String, vendor: String, desc: String,
                         shortDesc: String, offlineAllowedStr: String, jarsStr: String, propsStr: String, appDesc: String) = (
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
        <j2se version="1.5+ 1.6+ 1.7+" href="http://java.sun.com/products/autodl/j2se"/>""" +
        jarsStr + """

        <!-- System Properties -->""" +
        propsStr + """

    </resources>""" +
    appDesc + """
    <update check="timeout" policy="always"/>
</jnlp>
""").trim

}
