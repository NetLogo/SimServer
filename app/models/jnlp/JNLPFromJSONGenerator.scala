package models.jnlp

import
  scalaz.ValidationNel

import
  play.api.libs.json.JsValue

import
  models.filemanager.JNLPFileManager

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 12/13/12
 * Time: 3:53 PM
 */

object JNLPFromJSONGenerator {
  def apply(json: JsValue, host: String, extraProps: Map[String, String] = Map()) : ValidationNel[String, String] = {
    val filename     = JNLPFileManager.MyFolderName + "/" + java.util.UUID.randomUUID().toString + ".jnlp"
    val jnlpParamSet = JNLPParamSetManager.determineSet(json)
    val jnlpMaybe    = jnlpParamSet.bindFromJson(json, filename)(s"http://$host/assets") map (jnlp => jnlp.copy(properties = jnlp.properties ++ extraProps))
    jnlpMaybe map (jnlp => JNLPFileManager.registerFile(jnlp.toXMLStr.getBytes, filename).toString replaceAllLiterally("\\", "/"))
  }
}
