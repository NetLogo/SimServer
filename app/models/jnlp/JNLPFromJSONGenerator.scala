package models.jnlp

import
  java.util.UUID

import
  scalaz.ValidationNel

import
  play.api.libs.json.JsValue

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 12/13/12
 * Time: 3:53 PM
 */

object JNLPFromJSONGenerator {
  def apply(json: JsValue, host: String, extraProps: Map[String, String] = Map()) : ValidationNel[String, String] = {
    val filename     = s"${JNLPFileManager.MyFolderName}/${UUID.randomUUID().toString}.jnlp"
    val jnlpParamSet = JNLPParamSetManager.determineSet(json)
    val jnlpMaybe    = jnlpParamSet.bindFromJson(json, filename)(GenerationContext(s"http://$host/assets", s"http://$host/logging")) map (
      jnlp => jnlp.copy(properties = jnlp.properties ++ extraProps)
    )
    jnlpMaybe map (jnlp => JNLPFileManager.registerFile(jnlp.toXMLStr.getBytes, filename).toString replaceAllLiterally("\\", "/"))
  }
}
