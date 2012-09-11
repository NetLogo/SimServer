package models.jnlp

import play.api.libs.json.JsValue

import scalaz.Validation

import models.web.{ Param, ParamBox }
import models.util.Util.noneIfEmpty

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/10/12
 * Time: 3:05 PM
 */

object JNLPParams {

  private val RootDataKey = "data"  //@ Currently unused....  I feel like I did something wrong if I don't need this.  We'll see.

  private val CodebaseURIKey      = "codebase_uri"
  private val MainJarKey          = "main_jar"
  private val MainClassKey        = "main_class"
  private val ApplicationNameKey  = "application_name"
  private val DescKey             = "description"
  private val ShortDescKey        = "short_description"
  private val IsOfflineAllowedKey = "is_offline_allowed"
  private val AppNameInMenuKey    = "application_name_in_menu"
  private val VendorKey           = "vendor"
  private val DepsPathKey         = "dependencies_path"
  private val OtherJarsKey        = "other_jars"
  private val PropertiesKey       = "properties"
  private val ArgumentsKey        = "arguments"



  // -------------------> OTHER JARS SPECIFICS START <------------------- //

  private val OtherJarsArrElemKey       = "jar"
  private val OtherJarsArrElemNameKey   = "jar_name"
  private val OtherJarsArrElemIsLazyKey = "is_lazy"

  private def otherJarsParse(key: String)(js: JsValue) : Option[Seq[(String, Boolean)]] = {
    val jars         = (js \\ key) map (_ \ OtherJarsArrElemKey)
    val parsedJars   = jars map (jar => ((jar \ OtherJarsArrElemNameKey).asOpt[String], (jar \ OtherJarsArrElemIsLazyKey).asOpt[Boolean]))
    val cleansedJars = parsedJars collect { case (Some(jarStr), Some(isLazy)) => (jarStr, isLazy) }
    Option(cleansedJars)
  }

  private val OtherJarsParseDescriptor = """<root> ->
                                           |  <array_name = %s> -> (
                                           |    <object_name = %s> ->
                                           |      (<name = %s> -> <string>), (<name = %s> -> <boolean>)
                                           |  )*""".format(OtherJarsKey,
                                                           OtherJarsArrElemKey,
                                                           OtherJarsArrElemNameKey,
                                                           OtherJarsArrElemIsLazyKey).stripMargin

  // -------------------> OTHER JARS SPECIFICS END <------------------- //



  // -------------------> PROPERTIES SPECIFICS START <------------------- //

  private val PropertiesArrElemKey      = "property"
  private val PropertiesArrElemNameKey  = "name"
  private val PropertiesArrElemValueKey = "value"

  private def propertiesParse(key: String)(js: JsValue) : Option[Seq[(String, String)]] = {
    val properties    = (js \\ key) map (_ \ PropertiesArrElemKey)
    val parsedProps   = properties map (prop => ((prop \ PropertiesArrElemNameKey).asOpt[String], (prop \ PropertiesArrElemValueKey).asOpt[String]))
    val cleansedProps = parsedProps collect { case (Some(name), Some(value)) => (name, value) }
    Option(cleansedProps)
  }

  private val PropertiesParseDescriptor = """<root> ->
                                            |  <array_name = %s> -> (
                                            |    <object_name = %s> ->
                                            |      (<name = %s> -> <string>), (<name = %s> -> <string>)
                                            |  )*""".format(PropertiesKey,
                                                            PropertiesArrElemKey,
                                                            PropertiesArrElemNameKey,
                                                            PropertiesArrElemValueKey).stripMargin

  // -------------------> PROPERTIES SPECIFICS END <------------------- //



  // -------------------> ARGUMENTS SPECIFICS START <------------------- //

  private val ArgumentArrElemKey = "argument"

  private def argumentsParse(key: String)(js: JsValue) : Option[Seq[String]] = {
    val arguments    = js \\ key
    val parsedArgs   = arguments map (arg => arg.asOpt[String])
    val cleansedArgs = parsedArgs collect { case Some(arg) => arg }
    Option(cleansedArgs)
  }

  private val ArgumentsParseDescriptor = """<root> ->
                                           |  <array_name = %s> -> (<string>)*""".format(ArgumentArrElemKey).stripMargin

  // -------------------> ARGUMENTS SPECIFICS END <------------------- //


  private val CodebaseURIParam      = Param[String](CodebaseURIKey)
  private val MainJarParam          = Param[String](MainJarKey)
  private val MainClassParam        = Param[String](MainClassKey)
  private val ApplicationNameParam  = Param[String](ApplicationNameKey)
  private val DescParam             = Param[String](DescKey)
  private val ShortDescParam        = Param[String](ShortDescKey)
  private val IsOfflineAllowedParam = Param[Boolean](IsOfflineAllowedKey)
  private val AppNameInMenuParam    = Param[String](AppNameInMenuKey)
  private val VendorParam           = Param[String](VendorKey)
  private val DepsPathParam         = Param[String](DepsPathKey)
  private val OtherJarsParam        = Param[Seq[(String, Boolean)]](OtherJarsKey,  otherJarsParse  _, OtherJarsParseDescriptor)
  private val PropertiesParam       = Param[Seq[(String, String)]] (PropertiesKey, propertiesParse _, PropertiesParseDescriptor)
  private val ArgumentsParam        = Param[Seq[String]]           (ArgumentsKey,  argumentsParse  _, ArgumentsParseDescriptor)

  // --------------> Adding a param above?  THEN ADD IT TO THIS LIST! <-------------- //
  private val Params = Seq(CodebaseURIParam, MainJarParam, MainClassParam, ApplicationNameParam, DescParam,
                           ShortDescParam, IsOfflineAllowedParam, AppNameInMenuParam, VendorParam,
                           DepsPathParam, OtherJarsParam, PropertiesParam, ArgumentsParam)

  def stringify = Params map (param => "%s:\n\n%s".format(param.key, param.pathDescriptor)) mkString "\n\n\n\n"

  def bindFromJson(js: JsValue, jnlpLoc: String) : Validation[String, JNLP] =
    JNLP(
      CodebaseURIParam(js),
      ParamBox("** JNLP Location **", noneIfEmpty(jnlpLoc)), // If this fails validation... something is seriously messed up!
      MainJarParam(js),
      MainClassParam(js),
      ApplicationNameParam(js),
      DescParam(js),
      ShortDescParam(js),
      IsOfflineAllowedParam(js),
      AppNameInMenuParam(js),
      VendorParam(js),
      DepsPathParam(js),
      OtherJarsParam(js),
      PropertiesParam(js),
      ArgumentsParam(js)
    )

}






