package controllers

import play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import scalaz.{ Failure, Success }

import models.submission.{ SubmissionManager, SubmissionParser, Submittable }
import models.util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  def viewWork(period: String, run: String, user: String) = Action {
    val userWorks    = SubmissionManager.getUserWork(period, run, user)
    Ok(views.html.submissions(Seq(userWorks)))
//@    val withComments = userWorks map (work => work.addComments(SubmissionManager.getWorkComments(work): _*))
//    Ok(views.html.submissions(withComments))
  }

  private def submit[T <% Submittable](func: (Map[String, String] => Option[T]))(request: Request[AnyContent]) : SimpleResult[_] = {
    val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }
    val status = func(params) map {
      submittable =>
        val result = SubmissionManager.submit(submittable)
        Success(result)
    } getOrElse (Failure("Invalid POST data"))
    status fold ((ExpectationFailed(_)), (x => Ok(x.toString)))
  }

  def submitWork = APIAction {
    submit { SubmissionParser.parseOutUserWork(_) } _
  }

  def submitComment = APIAction {
    submit { SubmissionParser.parseOutWorkComment(_) } _
  }

  def submitSupplement = APIAction{
    submit { SubmissionParser.parseOutWorkSupplement(_) } _
  }

}
