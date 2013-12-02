package controllers.action

import
  play.api.mvc.{ Action, AnyContent, BodyParser, Request, Result }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/4/12
 * Time: 1:09 PM
 */

private[controllers] object APIAction extends OpenAction
private[controllers] object RestrictedAction {
  def apply(origins: Seq[String]) = new OpenAction {
//    override protected def APIDomains = origins
    //@ Currently, don't override, since play doesn't properly support multiple of the same header
  }
}

private[controllers] trait OpenAction {

  protected def APIDomains = Seq("*")

  protected def APIDomainHeaders  = APIDomains map ("Access-Control-Allow-Origin"  -> _)
  protected def APIReqTypesHeader = "Access-Control-Allow-Methods" -> "POST, GET, OPTIONS, PUT, DELETE"
  protected def APIControlsHeader = "Access-Control-Allow-Headers" -> "Content-Type"

  protected def APIHeaders = APIDomainHeaders :+ APIReqTypesHeader :+ APIControlsHeader

  protected def result2apiResult = (result: Result) => result.withHeaders(APIHeaders: _*)

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Result) = Action(bodyParser)(block andThen result2apiResult)
  def apply(block: Request[AnyContent] => Result)                      = Action(block andThen result2apiResult)
  def apply(block: => Result)                                          = Action(result2apiResult(block))

}

