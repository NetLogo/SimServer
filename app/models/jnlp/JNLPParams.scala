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
                                                                  |%s""".format(header, params).stripMargin
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
    val paramsStr = (bonusStringifyParams ++ additionalParams map paramFormat).mkString(Stream.fill(4)("\n").mkString)
    headerAndParamsFormat(headerStr, paramsStr)
  }

  protected def bonusStringifyParams : Seq[Param[_]] = Seq() // Should pretty much only be used by `BaseJNLPParams`
  protected def additionalParams     : Seq[Param[_]]
  protected def paramCategoryLabel   : String

  private[jnlp] def doesAffiliate(js: JsValue) : Boolean

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

  val ArgumentsArrKey = "arguments"

  def argumentsParse(key: String)(js: JsValue) : Option[Seq[String]] = (js \ key).asOpt[Seq[String]]

  val ArgumentsParseDescriptor = """<root> ->
                                   |  <array_name = %s> -> <string>*""".format(ArgumentsArrKey).stripMargin

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

  override val bonusStringifyParams = BaseParams
  override val additionalParams     = Seq[Param[_]]()
  override val paramCategoryLabel   = "Base"

  override private[jnlp] def doesAffiliate(js: JsValue) = false

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

object NetLogoParams extends JNLPParams {

  import JNLPParams._

  private val IsNetLogoKey   = "is_netlogo"
  private val IsNetLogoParam = Param[Boolean](IsNetLogoKey)

  override val additionalParams: Seq[Param[_]] = Seq(IsNetLogoParam)
  override val paramCategoryLabel              = "NetLogo"

  override private[jnlp] def doesAffiliate(js: JsValue) = IsNetLogoParam(js) is true

  override def bindFromJson(js: JsValue, jnlpLoc: String) : Validation[String, JNLP] =
    NetLogoJNLP(
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

object HubNetParams extends JNLPParams {

  import JNLPParams._

  private val IsHubNetServerKey   = "is_hubnet_server"
  private val IsHubNetServerParam = Param[Boolean](IsHubNetServerKey)

  private val IsHubNetClientKey   = "is_hubnet_client"
  private val IsHubNetClientParam = Param[Boolean](IsHubNetClientKey)

  private val ModelURLKey   = "model_url"
  private val ModelURLParam = Param[String](ModelURLKey)

  private val ProgramNameKey   = "program_name"
  private val ProgramNameParam = Param[String](ProgramNameKey)

  private val RoleKey   = "role"
  private val RoleParam = Param[String](RoleKey)

  private val ServerIPKey   = "server_ip"
  private val ServerIPParam = Param[String](ServerIPKey)

  private val ServerPortKey   = "server_port"
  private val ServerPortParam = Param[Int](ServerPortKey)

  private val UserIDKey   = "user_id"
  private val UserIDParam = Param[String](UserIDKey)

  override private[jnlp] def doesAffiliate(js: JsValue) = (IsHubNetServerParam(js) is true) || (IsHubNetClientParam(js) is true)

  override val additionalParams   = Seq(IsHubNetClientParam, IsHubNetServerParam, ModelURLParam, ProgramNameParam,
                                        RoleParam, ServerIPParam, ServerPortParam, UserIDParam)
  override val paramCategoryLabel = "HubNet"

  override def bindFromJson(js: JsValue, jnlpLoc: String) : Validation[String, JNLP] =
    HubNetJNLP(
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
      ArgumentsParam(js),
      ProgramNameParam(js),
      RoleParam(js),
      IsHubNetServerParam(js),
      ModelURLParam(js),
      ServerIPParam(js),
      ServerPortParam(js),
      UserIDParam(js)
    )

}

object JNLPParamSetManager {
  private val paramTypes = Seq(BaseJNLPParams, NetLogoParams, HubNetParams)
  private val default    = BaseJNLPParams
  def determineSet(js: JsValue) : JNLPParams = paramTypes find (_.doesAffiliate(js)) getOrElse default
  def stringifySets                          = paramTypes map (_.stringify) mkString(Stream.fill(4)("\n").mkString)
}