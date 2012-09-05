package controllers

import play.api.mvc.{ Action, AnyContent, BodyParser, Request, SimpleResult }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/4/12
 * Time: 1:09 PM
 */

private[controllers] object APIAction {

  private val APIHeader = "Access-Control-Allow-Origin" -> "*"

  private val result2apiResult = (result: SimpleResult[_]) => result.withHeaders(APIHeader)

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => SimpleResult[_]) = Action(bodyParser)(block andThen result2apiResult)
  def apply(block: Request[AnyContent] => SimpleResult[_])                      = Action(block andThen result2apiResult)
  def apply(block: => SimpleResult[_])                                          = Action(result2apiResult(block))

}

