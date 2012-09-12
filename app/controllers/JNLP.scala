package controllers

import play.api.mvc.Controller

import models.jnlp.JNLPParams
import models.filemanager.TempFileManager

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/7/12
 * Time: 1:18 PM
 */

object JNLP extends Controller {

  def generateJNLP = APIAction {
    request =>
      request.body.asJson map {
        json =>
          val filename  = TempFileManager.formatFilePath(json.toString, "jnlp")
          val jnlpMaybe = JNLPParams.bindFromJson(json, filename)
          val strMaybe  = jnlpMaybe map (jnlp => TempFileManager.registerFile(jnlp.toXMLStr, filename).toString replaceAllLiterally("\\", "/"))
          strMaybe fold((ExpectationFailed(_)), (url => Redirect("/" + url)))
      } getOrElse BadRequest("Invalid POST body; expected a JSON object")
  }

  def getParamFormats = APIAction {
    Ok(JNLPParams.stringify)
  }

}
