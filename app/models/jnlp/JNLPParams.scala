package models.jnlp

import play.api.libs.json.JsValue

import scalaz.ValidationNEL

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
    def headerAndParamsFormat(header: String, params: String) = s"""$header
                                                                  |
                                                                  |$params""".stripMargin
    def headerFormat(header: String) = {
      val barrier = Stream.fill(header.length)("=").mkString
     s"""$barrier
        |$header
        |$barrier"""
    }
    def paramFormat(param: Param[_]) = s"""${param.key}
                                          |
                                          |${param.pathDescriptor}""".stripMargin
    val headerStr = headerFormat(paramCategoryLabel.toUpperCase)
    val paramsStr = (bonusStringifyParams ++ additionalParams map paramFormat).mkString(Stream.fill(4)("\n").mkString)
    headerAndParamsFormat(headerStr, paramsStr)
  }

  protected def bonusStringifyParams : Seq[Param[_]] = Seq() // Should pretty much only be used by `BaseJNLPParams`
  protected def additionalParams     : Seq[Param[_]]
  protected def paramCategoryLabel   : String

  private[jnlp] def doesAffiliate(js: JsValue) : Boolean

  def bindFromJson(js: JsValue, jnlpLoc: String)(implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP]

}

object JNLPKeys {

  val CodebaseURLKey      = "codebase_url"
  val MainJarKey          = "main_jar"
  val MainClassKey        = "main_class"
  val ApplicationNameKey  = "application_name"
  val DescKey             = "description"
  val ShortDescKey        = "short_description"
  val IsOfflineAllowedKey = "is_offline_allowed"
  val AppNameInMenuKey    = "application_name_in_menu"
  val VendorKey           = "vendor"
  val DepsPathKey         = "dependencies_path"
  val VMArgsKey           = "vm_args"
  val OtherJarsKey        = "other_jars"
  val PropertiesKey       = "properties"
  val ArgumentsKey        = "arguments"

  val OtherJarsArrElemNameKey   = "jar_name"
  val OtherJarsArrElemIsLazyKey = "is_lazy"

  val PropertiesArrElemNameKey  = "name"
  val PropertiesArrElemValueKey = "value"

  val ArgumentsArrKey = "arguments"

}

private[jnlp] object JNLPParams {

  import JNLPKeys._

  def parseJsArray[T, U](key: String)(js: JsValue)(parseFunc: JsValue => T)(validationFunc: PartialFunction[T, U]) : Option[Seq[U]] =
    (js \ key).asOpt[Seq[JsValue]] map { _ map parseFunc collect validationFunc }


  // -------------------> OTHER JARS SPECIFICS START <------------------- //

  def otherJarsParse(key: String)(js: JsValue) : Option[Seq[(String, Boolean)]] = {
    val parseFunc = (jar: JsValue) => ((jar \ OtherJarsArrElemNameKey).asOpt[String], (jar \ OtherJarsArrElemIsLazyKey).asOpt[Boolean])
    parseJsArray(key)(js)(parseFunc){ case (Some(jarStr), Some(isLazy)) => (jarStr, isLazy) }
  }

  val OtherJarsParseDescriptor = s"""<root> ->
                                    |  <array_name = $OtherJarsKey> -> {
                                    |    (<name = $OtherJarsArrElemNameKey> -> <string>), (<name = $OtherJarsArrElemIsLazyKey> -> <boolean>)
                                    |  }*""".stripMargin

  // -------------------> OTHER JARS SPECIFICS END <------------------- //



  // -------------------> PROPERTIES SPECIFICS START <------------------- //

  def propertiesParse(key: String)(js: JsValue) : Option[Seq[(String, String)]] = {
    val parseFunc = (prop: JsValue) => ((prop \ PropertiesArrElemNameKey).asOpt[String], (prop \ PropertiesArrElemValueKey).asOpt[String])
    parseJsArray(key)(js)(parseFunc){ case (Some(name), Some(value)) => (name, value) }
  }

  val PropertiesParseDescriptor = s"""<root> ->
                                     |  <array_name = $PropertiesKey> -> {
                                     |    (<name = $PropertiesArrElemNameKey> -> <string>), (<name = $PropertiesArrElemValueKey> -> <string>)
                                     |  }*""".stripMargin

  // -------------------> PROPERTIES SPECIFICS END <------------------- //



  // -------------------> ARGUMENTS SPECIFICS START <------------------- //

  def argumentsParse(key: String)(js: JsValue) : Option[Seq[String]] = (js \ key).asOpt[Seq[String]]

  val ArgumentsParseDescriptor = s"""<root> ->
                                    |  <array_name = $ArgumentsArrKey> -> <string>*""".stripMargin

  // -------------------> ARGUMENTS SPECIFICS END <------------------- //


  val CodebaseURLParam      = Param[String](CodebaseURLKey)
  val MainJarParam          = Param[String](MainJarKey)
  val MainClassParam        = Param[String](MainClassKey)
  val ApplicationNameParam  = Param[String](ApplicationNameKey)
  val DescParam             = Param[String](DescKey)
  val ShortDescParam        = Param[String](ShortDescKey)
  val IsOfflineAllowedParam = Param[Boolean](IsOfflineAllowedKey)
  val AppNameInMenuParam    = Param[String](AppNameInMenuKey)
  val VendorParam           = Param[String](VendorKey)
  val DepsPathParam         = Param[String](DepsPathKey)
  val VMArgsParam           = Param[String](VMArgsKey)
  val OtherJarsParam        = Param[Seq[(String, Boolean)]](OtherJarsKey,  otherJarsParse  _, OtherJarsParseDescriptor)
  val PropertiesParam       = Param[Seq[(String, String)]] (PropertiesKey, propertiesParse _, PropertiesParseDescriptor)
  val ArgumentsParam        = Param[Seq[String]]           (ArgumentsKey,  argumentsParse  _, ArgumentsParseDescriptor)

  // --------------> Adding a param above?  THEN ADD IT TO THIS LIST! <-------------- //
  val BaseParams = Seq(CodebaseURLParam, MainJarParam, MainClassParam, ApplicationNameParam, DescParam,
                       ShortDescParam, IsOfflineAllowedParam, AppNameInMenuParam, VendorParam,
                       DepsPathParam, VMArgsParam, OtherJarsParam, PropertiesParam, ArgumentsParam)

}

object BaseJNLPParams extends JNLPParams {

  import JNLPParams._

  override val bonusStringifyParams = BaseParams
  override val additionalParams     = Seq[Param[_]]()
  override val paramCategoryLabel   = "Base"

  override private[jnlp] def doesAffiliate(js: JsValue) = false

  override def bindFromJson(js: JsValue, jnlpLoc: String)(implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP] =
    JNLP(
      CodebaseURLParam(js),
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
      VMArgsParam(js),
      OtherJarsParam(js),
      PropertiesParam(js),
      ArgumentsParam(js)
    )

}

object NetLogoKeys {
  val IsNetLogoKey      = "is_netlogo"
  val ModelURLKey       = "model_url"
  val UsesExtensionsKey = "uses_extensions"
}

object NetLogoParams extends JNLPParams {

  import JNLPParams._, NetLogoKeys._

  private       val IsNetLogoParam      = Param[Boolean](IsNetLogoKey)
  private[jnlp] val ModelURLParam       = Param[String](ModelURLKey)
  private[jnlp] val UsesExtensionsParam = Param[Boolean](UsesExtensionsKey)

  override val additionalParams: Seq[Param[_]] = Seq(IsNetLogoParam, ModelURLParam, UsesExtensionsParam)
  override val paramCategoryLabel              = "NetLogo"

  override private[jnlp] def doesAffiliate(js: JsValue) = IsNetLogoParam(js) is true

  override def bindFromJson(js: JsValue, jnlpLoc: String)(implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP] =
    NetLogoJNLP(
      CodebaseURLParam(js),
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
      VMArgsParam(js),
      OtherJarsParam(js),
      ModelURLParam(js),
      UsesExtensionsParam(js),
      PropertiesParam(js),
      ArgumentsParam(js)
    )

}

object HubNetKeys {
  val IsHubNetServerKey = "is_hubnet_server"
  val IsHubNetClientKey = "is_hubnet_client"
  val ProgramNameKey    = "program_name"
  val RoleKey           = "role"
  val ServerIPKey       = "server_ip"
  val ServerPortKey     = "server_port"
  val UserIDKey         = "user_id"
}

object HubNetParams extends JNLPParams {

  import JNLPParams._, HubNetKeys._

  private val IsHubNetServerParam = Param[Boolean](IsHubNetServerKey)
  private val IsHubNetClientParam = Param[Boolean](IsHubNetClientKey)
  private val ProgramNameParam    = Param[String](ProgramNameKey)
  private val RoleParam           = Param[String](RoleKey)
  private val ServerIPParam       = Param[String](ServerIPKey)
  private val ServerPortParam     = Param[Int](ServerPortKey)
  private val UserIDParam         = Param[String](UserIDKey)

  override private[jnlp] def doesAffiliate(js: JsValue) = (IsHubNetServerParam(js) is true) || (IsHubNetClientParam(js) is true)

  override val additionalParams   = Seq(IsHubNetClientParam, IsHubNetServerParam, ProgramNameParam,
                                        RoleParam, ServerIPParam, ServerPortParam, UserIDParam)
  override val paramCategoryLabel = "HubNet"

  override def bindFromJson(js: JsValue, jnlpLoc: String)(implicit thisServerCodebaseURL: String) : ValidationNEL[String, JNLP] =
    HubNetJNLP(
      CodebaseURLParam(js),
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
      VMArgsParam(js),
      OtherJarsParam(js),
      PropertiesParam(js),
      ArgumentsParam(js),
      ProgramNameParam(js)
    )(
      RoleParam(js),
      IsHubNetServerParam(js) orElse (IsHubNetClientParam(js) map (!_)),
      NetLogoParams.ModelURLParam(js),
      NetLogoParams.UsesExtensionsParam(js),
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
