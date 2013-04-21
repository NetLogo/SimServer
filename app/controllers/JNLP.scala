package controllers

import
  play.api.mvc.{ AnyContent, Controller, Request }

import
  controllers.action.APIAction

import
  models.{ jnlp, util },
    jnlp.{ JNLPFromJSONGenerator, JNLPParamSetManager },
    util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/7/12
 * Time: 1:18 PM
 */

object JNLP extends Controller {

  def generateJNLP = APIAction {
    request =>
      PlayUtil.extractJSONOpt(request) map (JNLPFromJSONGenerator(_, request.host)) map {
        _ fold((nel => ExpectationFailed(nel.list.mkString("\n"))), (url => Ok(s"http://${request.host}/$url")))
      } getOrElse BadRequest("Invalid POST body; expected a JSON object")
  }

  def getParamFormats = APIAction {
    Ok(JNLPParamSetManager.stringifySets)
  }

}
