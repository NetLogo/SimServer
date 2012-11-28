package controllers

import play.api.mvc.Controller

import models.filemanager.TempFileManager
import models.jnlp.JNLPParamSetManager
import models.util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/7/12
 * Time: 1:18 PM
 */

object JNLP extends Controller {

  def generateJNLP = APIAction {
    request =>
      PlayUtil.extractJSONOpt(request) map {
        json =>
          val filename     = TempFileManager.formatFilePath(json.toString, "jnlp")
          val jnlpParamSet = JNLPParamSetManager.determineSet(json)
          val jnlpMaybe    = jnlpParamSet.bindFromJson(json, filename)
          val strMaybe     = jnlpMaybe map (jnlp => TempFileManager.registerFile(jnlp.toXMLStr, filename).toString replaceAllLiterally("\\", "/"))
          strMaybe fold((nel => ExpectationFailed(nel.list.mkString("\n"))), (url => Ok("http://%s/%s".format(request.host, url))))
      } getOrElse BadRequest("Invalid POST body; expected a JSON object")
  }

  def getParamFormats = APIAction {
    Ok(JNLPParamSetManager.stringifySets)
  }

}
