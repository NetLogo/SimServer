package models.jnlp

import play.api.libs.json.JsValue

import scalaz.ValidationNEL

import models.filemanager.TempFileManager

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 12/13/12
 * Time: 3:53 PM
 */

object JNLPFromJSONGenerator {
  def apply(json: JsValue, host: String) : ValidationNEL[String, String] = {
    val filename     = TempFileManager.formatFilePath(json.toString, "jnlp")
    val jnlpParamSet = JNLPParamSetManager.determineSet(json)
    val jnlpMaybe    = jnlpParamSet.bindFromJson(json, filename)("http://%s/assets".format(host))
    jnlpMaybe map (jnlp => TempFileManager.registerFile(jnlp.toXMLStr, filename).toString replaceAllLiterally("\\", "/"))
  }
}
