package controllers

import play.api.mvc.{ Action, AnyContent, BodyParser, Request, Result, SimpleResult }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/4/12
 * Time: 1:09 PM
 */

object APIAction {

  private val APIHeader = "Access-Control-Allow-Origin" -> "*"

  private val result2apiResult = (result: Result) => {
    result match {
      case simple: SimpleResult[_] => simple.withHeaders(APIHeader)
      case x                       => x
    }
  }

  def apply[A](bodyParser: BodyParser[A])(block: Request[A] => Result) = Action(bodyParser)(block andThen result2apiResult)
  def apply(block: Request[AnyContent] => Result)                      = Action(block andThen result2apiResult)
  def apply(block: => Result)                                          = Action(result2apiResult(block))

}

