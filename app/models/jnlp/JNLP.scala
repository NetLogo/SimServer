package models.jnlp

import java.net.URI

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 3:40 PM
 */

case class JNLP(
 /* Required */ serverPublicURI: URI,            // Should just be the 'public' folder
 /* Required */ jnlpLoc: String,
 /* Required */ mainJar: MainJar,
 /* Required */ applicationName: String,
 /* Required */ mainClass: String,
                appTitle: String                      = "NetLogo",
                desc: String                          = "A NetLogo WebStart app",
                shortDesc: String                     = "NetLogo (WebStart)",
                isOfflineAllowed: Boolean             = true,
                appNameInMenu: String                 = "NetLogo (WebStart)",
                otherJars: Seq[Jar]                   = Seq(),
                properties: Seq[Pair[String, String]] = Seq(),
                arguments: Seq[String]                = Seq()
               ) {

  val DepsDir = "misc/deps"

  def toXMLStr : String = {

    val jars = mainJar +: otherJars
    val props = properties // No additions as of right now
    val args = arguments   // Currently, no additions

    val offlineAllowedStr = if (isOfflineAllowed) "\n" + formatXMLNode("offline-allowed", Map(), 2) else ""
    val jarsStr = jars map { jar => import jar._; formatXMLNode("jar", Map((Seq("href" -> "%s/%s".format(DepsDir, jarName)) ++ (if (isMain) Seq("main" -> "true") else Seq()) ++ (if (isLazy) Seq("download" -> "lazy") else Seq())): _*), 2) } mkString("\n", "\n", "")
    val propsStr = props map { case (key, value) => formatXMLNode("property", Map("name" -> key, "value" -> value), 2) } mkString("\n", "\n", "")
    val argsStr = args map (formatXMLPair("argument", Map(), _, 2)) mkString("\n", "\n", "\n" + formatIndentation(1))
    val appDescStr = "\n" + formatXMLPair("application-desc", Map("name" -> applicationName, "main-class" -> mainClass), argsStr, 1)

// It's tempting to use `String.format` here, but I fear that it would get far too confusing
("""
<?xml version="1.0" encoding="UTF-8"?>
<jnlp spec="1.0+" codebase=""" + '"' + serverPublicURI.toString + "/assets" + '"' + """ href=""" + '"' + jnlpLoc + '"' + """>
    <information>
        <title>""" + appTitle + """</title>
        <vendor>CCL</vendor>
        <description>""" + desc + """</description>
        <description kind="short">""" + shortDesc + """</description>""" + offlineAllowedStr + """
        <shortcut online="false">
            <desktop/>
            <menu submenu=""" + '"' + appNameInMenu + '"' + """/>
        </shortcut>
    </information>
    <security> <all-permissions/> </security>
    <resources>

        <!-- Application Resources -->
        <j2se version="1.5+ 1.6+ 1.7+" href="http://java.sun.com/products/autodl/j2se"/>
        <jar href=""" + '"' + DepsDir + """/extensions.jar" download="lazy"/>
        <jar href=""" + '"' + DepsDir + """/logging.jar" download="lazy"/>
        <jar href=""" + '"' + DepsDir + """/asm-all-3.3.1.jar"/>
        <jar href=""" + '"' + DepsDir + """/gluegen-rt-1.1.1.jar"/>
        <jar href=""" + '"' + DepsDir + """/jhotdraw-6.0b1.jar"/>
        <jar href=""" + '"' + DepsDir + """/jmf-2.1.1e.jar"/>
        <jar href=""" + '"' + DepsDir + """/jogl-1.1.1.jar"/>
        <jar href=""" + '"' + DepsDir + """/log4j-1.2.16.jar"/>
        <jar href=""" + '"' + DepsDir + """/mrjadapter-1.2.jar"/>
        <jar href=""" + '"' + DepsDir + """/picocontainer-2.13.6.jar"/>
        <jar href=""" + '"' + DepsDir + """/quaqua-7.3.4.jar"/>
        <jar href=""" + '"' + DepsDir + """/scala-library.jar"/>
        <jar href=""" + '"' + DepsDir + """/swing-layout-7.3.4.jar"/>""" + jarsStr + """

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
                             { val attrs = formatAttrs(attrKVs); if (attrs.isEmpty) "" else " " + attrs } , data, tagName)
  }

  private def formatAttrs(attrKVs: Map[String, String]) : String = {
    attrKVs.toList map { case (key, value) => "%s=\"%s\"".format(key, value) } mkString " "
  }

  private def formatIndentation(level: Int) = (Stream continually " " take (level * 4)).mkString
  
}
