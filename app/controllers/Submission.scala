package controllers

import play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import scalaz.{ Success, Validation }

import models.submission.{ SubmissionManager, Submittable }
import models.util.PlayUtil
import models.parse.submission.{ CommentParser, SupplementParser, WorkParser }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  def viewWork(period: String, run: String, user: String) = Action {
    val userWorks = SubmissionManager.getUserWork(period, run, user)
    Ok(views.html.submissions(userWorks))
  }

  def updateAndViewWork(period: String, run: String, user: String) = Action {
    (submit { CommentParser(_) } _) andThen {
      case x if (x.header.status == OK) => Redirect(routes.Submission.viewWork(period, run, user))
      case x                            => x
    }
  }

  private def submit[T <% Submittable](func: (Map[String, String] => Validation[String, T]))(request: Request[AnyContent]) : SimpleResult[_] = {
    val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }
    func(params) map {
      submittable =>
        val result = SubmissionManager.submit(submittable)
        Success(result)
    } fold ((ExpectationFailed(_)), (x => Ok(x.toString)))
  }

  def submitWork = APIAction {
    submit { WorkParser(_) } _
  }

  def submitComment = APIAction {
    submit { CommentParser(_) } _
  }

  def submitSupplement = APIAction{
    submit { SupplementParser(_) } _
  }

}
