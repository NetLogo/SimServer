package controllers

import play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import scalaz.{ Success, Validation }

import models.submission.{ SubmissionManager, Submittable, UserWork, UserWorkComment, UserWorkSupplement }
import models.util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  def viewWork(period: String, run: String, user: String) = Action {
    val userWorks = SubmissionManager.getUserWork(period, run, user)
    val js = userWorks.map(_.typ).distinct.map {
      name =>
        val funcBody = SubmissionManager.getTypeBundleByName(name) map (_.js) getOrElse (generateDefaultJS(name))
        finalizeJS(funcBody, name)
    }.mkString
    Ok(views.html.submissions(userWorks, js))
  }

  def updateAndViewWork(period: String, run: String, user: String) = Action {
    (submit { UserWorkComment.fromMap(_) } _) andThen {
      case x if (x.header.status == OK) => Redirect(routes.Submission.viewWork(period, run, user))
      case x                            => x
    }
  }

  private def finalizeJS(funcBody: String, name: String) : String =
    """
      function do_custom_%s() {
        %s
      }
    """.format(name, funcBody)

  private def generateDefaultJS(name: String) : String = """alert("No action defined for content type '%s'");""".format(name)

  private def submit[T <% Submittable](func: (Map[String, String] => Validation[String, T]))(request: Request[AnyContent]) : SimpleResult[_] = {
    val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }
    func(params) map {
      submittable =>
        val result = SubmissionManager.submit(submittable)
        Success(result)
    } fold ((ExpectationFailed(_)), (x => Ok(x.toString)))
  }

  def submitWork = APIAction {
    submit { UserWork.fromMap(_) } _
  }

  def submitComment = APIAction {
    submit { UserWorkComment.fromMap(_) } _
  }

  def submitSupplement = APIAction{
    submit { UserWorkSupplement.fromMap(_) } _
  }

}
