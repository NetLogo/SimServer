package controllers.action

import
  play.api.mvc.{ Action, AnyContent, BodyParser, Request, SimpleResult }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/4/12
 * Time: 1:09 PM
 */

private[controllers] object APIAction extends OpenAction
private[controllers] object RestrictedAction {
  def apply(origins: Seq[String]) = new OpenAction {
    override protected val APIDomains = origins
  }
}

private[controllers] trait OpenAction {

  protected val APIDomains = Seq("*")

  protected val APIDomainHeaders  = APIDomains map ("Access-Control-Allow-Origin"  -> _)
  protected val APIReqTypesHeader = "Access-Control-Allow-Methods" -> "POST, GET, OPTIONS, PUT, DELETE"
  protected val APIControlsHeader = "Access-Control-Allow-Headers" -> "Content-Type"

  protected val APIHeaders = APIDomainHeaders :+ APIReqTypesHeader :+ APIControlsHeader

  protected val result2apiResult = (result: SimpleResult[_]) => result.withHeaders(APIHeaders: _*)

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => SimpleResult[_]) = Action(bodyParser)(block andThen result2apiResult)
  def apply(block: Request[AnyContent] => SimpleResult[_])                      = Action(block andThen result2apiResult)
  def apply(block: => SimpleResult[_])                                          = Action(result2apiResult(block))

}

