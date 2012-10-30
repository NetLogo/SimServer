package controllers

import play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import scalaz.{ Success, Validation }

import models.submission.{ SubmissionManager, Submittable, TypeBundle, UserWork, UserWorkComment, UserWorkSupplement }
import models.util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  //@ Ensure that the 'uploads' folder exists on init

  def viewTypeCreationForm = Action {
    Ok(views.html.create_sub_type())
  }

  def createType = Action {
    request =>
      val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) } //@ Unify this code
      val bundle = TypeBundle(params("name"), "") //@ Validate better
      SubmissionManager.submit(bundle)
      Redirect(routes.Submission.viewTypeEditForm(bundle.name))
  }

  def viewTypeEditForm(name: String) = Action {
    request =>
      val bundleOpt = SubmissionManager.getTypeBundleByName(name)
      Ok(views.html.edit_sub_type(bundleOpt.get)) //@ Validate better
  }

  def editType(name: String) = Action {
    request =>
      val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) } //@ Unify this code
      val bundle = TypeBundle(name, params("js")) //@ Validate better
      SubmissionManager.update(bundle)
      Redirect(routes.Submission.viewTypeEditForm(name))
  }

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
      function do_custom_%s(data) {
        %s
      }
    """.format(name, funcBody)

  private def generateDefaultJS(name: String) : String = """alert("No action defined for content type '%s'");""".format(name)

  private def submit[T <% Submittable](f: (Map[String, String] => Validation[String, T]))(request: Request[AnyContent]) : SimpleResult[_] = {
    val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }
    f(params) map {
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

  def submitSupplement = APIAction {
    submit { UserWorkSupplement.fromMap(_) } _
  }

}
