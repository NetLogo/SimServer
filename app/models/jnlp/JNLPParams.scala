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

trait JNLPParams {

  final def stringify : String = {
    def headerAndParamsFormat(header: String, params: String) = """%s
                                                                  |
                                                                  |%s
                                                                  |
                                                                  |
                                                                  |
                                                                  |""".format(header, params).stripMargin
    def headerFormat(header: String) = {
      val barrier = Stream.fill(header.length)("=").mkString
      """%s
        |%s
        |%s""".format(barrier, header, barrier)
    }
    def paramFormat(param: Param[_]) = """%s
                                         |
                                         |%s""".format(param.key, param.pathDescriptor).stripMargin
    val headerStr = headerFormat(paramCategoryLabel.toUpperCase)
    val paramsStr = (JNLPParams.BaseParams ++ additionalParams map paramFormat).mkString
    headerAndParamsFormat(headerStr, paramsStr)
  }

  protected def additionalParams   : Seq[Param[_]]
  protected def paramCategoryLabel : String

  def bindFromJson(js: JsValue, jnlpLoc: String) : Validation[String, JNLP]

}

private[jnlp] object JNLPParams {

  val CodebaseURIKey      = "codebase_uri"
  val MainJarKey          = "main_jar"
  val MainClassKey        = "main_class"
  val ApplicationNameKey  = "application_name"
  val DescKey             = "description"
  val ShortDescKey        = "short_description"
  val IsOfflineAllowedKey = "is_offline_allowed"
  val AppNameInMenuKey    = "application_name_in_menu"
  val VendorKey           = "vendor"
  val DepsPathKey         = "dependencies_path"
  val OtherJarsKey        = "other_jars"
  val PropertiesKey       = "properties"
  val ArgumentsKey        = "arguments"

  def parseJsArray[T, U](key: String)(js: JsValue)(parseFunc: JsValue => T)(validationFunc: PartialFunction[T, U]) : Option[Seq[U]] =
    (js \ key).asOpt[Seq[JsValue]] map { _ map parseFunc collect validationFunc }


  // -------------------> OTHER JARS SPECIFICS START <------------------- //

  val OtherJarsArrElemNameKey   = "jar_name"
  val OtherJarsArrElemIsLazyKey = "is_lazy"

  def otherJarsParse(key: String)(js: JsValue) : Option[Seq[(String, Boolean)]] = {
    val parseFunc = (jar: JsValue) => ((jar \ OtherJarsArrElemNameKey).asOpt[String], (jar \ OtherJarsArrElemIsLazyKey).asOpt[Boolean])
    parseJsArray(key)(js)(parseFunc){ case (Some(jarStr), Some(isLazy)) => (jarStr, isLazy) }
  }

  val OtherJarsParseDescriptor = """<root> ->
                                   |  <array_name = %s> -> {
                                   |    (<name = %s> -> <string>), (<name = %s> -> <boolean>)
                                   |  }*""".format(OtherJarsKey,
                                                   OtherJarsArrElemNameKey,
                                                   OtherJarsArrElemIsLazyKey).stripMargin

  // -------------------> OTHER JARS SPECIFICS END <------------------- //



  // -------------------> PROPERTIES SPECIFICS START <------------------- //

  val PropertiesArrElemNameKey  = "name"
  val PropertiesArrElemValueKey = "value"

  def propertiesParse(key: String)(js: JsValue) : Option[Seq[(String, String)]] = {
    val parseFunc = (prop: JsValue) => ((prop \ PropertiesArrElemNameKey).asOpt[String], (prop \ PropertiesArrElemValueKey).asOpt[String])
    parseJsArray(key)(js)(parseFunc){ case (Some(name), Some(value)) => (name, value) }
  }

  val PropertiesParseDescriptor = """<root> ->
                                    |  <array_name = %s> -> {
                                    |    (<name = %s> -> <string>), (<name = %s> -> <string>)
                                    |  }*""".format(PropertiesKey,
                                                    PropertiesArrElemNameKey,
                                                    PropertiesArrElemValueKey).stripMargin

  // -------------------> PROPERTIES SPECIFICS END <------------------- //



  // -------------------> ARGUMENTS SPECIFICS START <------------------- //

  val ArgumentArrElemKey = "argument"

  def argumentsParse(key: String)(js: JsValue) : Option[Seq[String]] = (js \ key).asOpt[Seq[String]]

  val ArgumentsParseDescriptor = """<root> ->
                                   |  <array_name = %s> -> <string>*""".format(ArgumentArrElemKey).stripMargin

  // -------------------> ARGUMENTS SPECIFICS END <------------------- //


  val CodebaseURIParam      = Param[String](CodebaseURIKey)
  val MainJarParam          = Param[String](MainJarKey)
  val MainClassParam        = Param[String](MainClassKey)
  val ApplicationNameParam  = Param[String](ApplicationNameKey)
  val DescParam             = Param[String](DescKey)
  val ShortDescParam        = Param[String](ShortDescKey)
  val IsOfflineAllowedParam = Param[Boolean](IsOfflineAllowedKey)
  val AppNameInMenuParam    = Param[String](AppNameInMenuKey)
  val VendorParam           = Param[String](VendorKey)
  val DepsPathParam         = Param[String](DepsPathKey)
  val OtherJarsParam        = Param[Seq[(String, Boolean)]](OtherJarsKey,  otherJarsParse  _, OtherJarsParseDescriptor)
  val PropertiesParam       = Param[Seq[(String, String)]] (PropertiesKey, propertiesParse _, PropertiesParseDescriptor)
  val ArgumentsParam        = Param[Seq[String]]           (ArgumentsKey,  argumentsParse  _, ArgumentsParseDescriptor)

  // --------------> Adding a param above?  THEN ADD IT TO THIS LIST! <-------------- //
  val BaseParams = Seq(CodebaseURIParam, MainJarParam, MainClassParam, ApplicationNameParam, DescParam,
                       ShortDescParam, IsOfflineAllowedParam, AppNameInMenuParam, VendorParam,
                       DepsPathParam, OtherJarsParam, PropertiesParam, ArgumentsParam)

}

object BaseJNLPParams extends JNLPParams {

  import JNLPParams._

  override val additionalParams   = Seq[Param[_]]()
  override val paramCategoryLabel = "Base"

  override def bindFromJson(js: JsValue, jnlpLoc: String) : Validation[String, JNLP] =
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



