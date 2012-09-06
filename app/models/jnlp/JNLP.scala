package models.jnlp

import java.net.URI

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
 /* Required */ applicationName: String,
 /* Required */ mainClass: String,
                appTitle: String                      = "NetLogo",
                desc: String                          = "A NetLogo WebStart app",
                shortDesc: String                     = "NetLogo (WebStart)",
                isOfflineAllowed: Boolean             = true,
                appNameInMenu: String                 = "NetLogo (WebStart)", // Used if we ever want to save application shortcuts
                vendor: String                        = "CCL",
                depsPath: String                      = "misc/deps",
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
    val appDescStr = "\n" + formatXMLPair("application-desc", Map("name" -> applicationName, "main-class" -> mainClass), argsStr, 1)
    val jarsStr = jars map {
      jar =>
        import jar._
        val hrefProps =
          Seq("href" -> "%s/%s".format(depsPath, jarName)) ++
          (if (isMain) Seq("main" -> "true") else Seq()) ++
          (if (isLazy) Seq("download" -> "lazy") else Seq())
        formatXMLNode("jar", Map(hrefProps: _*), 2)
    } mkString ("\n", "\n", "")

// It's tempting to use `String.format` here, but I fear that it would get far too confusing
// (Also, Guns N' Roses once wrote a song about the following code; it was called "Welcome to the Jungle")
("""
<?xml version="1.0" encoding="UTF-8"?>
<jnlp spec="1.0+" codebase=""" + '"' + codebaseURI.toString + '"' + """ href=""" + '"' + jnlpLoc + '"' + """>
    <information>
        <title>""" + appTitle + """</title>
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
    appDescStr + """
    <update check="timeout" policy="always"/>
</jnlp>
""").trim

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
