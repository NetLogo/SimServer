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
    NotFound("Page temporarily disabled due to heavy load.") // handleJNLPGeneration(_)
  }

  def getParamFormats = APIAction {
    Ok(JNLPParamSetManager.stringifySets)
  }

  private[controllers] def handleJNLPGeneration(request: Request[AnyContent], extraProps: Map[String, String] = Map()) = {
    PlayUtil.extractJSONOpt(request) map (JNLPFromJSONGenerator(_, request.host, extraProps)) map {
      _ fold(nel => ExpectationFailed(nel.list.mkString("\n")), url => Ok(s"http://${request.host}/$url"))
    } getOrElse BadRequest("Invalid POST body; expected a JSON object")
  }

}
