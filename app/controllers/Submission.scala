package controllers

import play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import scalaz.{ Success, Validation }

import models.submission._
import models.util.PlayUtil

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  private val noCleanup = ((_: (_, Validation[String, Long]))._2)

  //@ Ensure that the 'uploads' folder exists on init

  def viewTypeCreationForm = Action {
    Ok(views.html.create_sub_type())
  }

  def createType = Action {
    request =>
      val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) } //@ Unify this code
      val bundle = TypeBundle(params("name"), "", "", "") //@ Validate better
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
      val bundle = TypeBundle(name, params("action_js"), params("presentation_js"), params("file_extension")) //@ Validate better
      SubmissionManager.update(bundle)
      Redirect(routes.Submission.viewTypeEditForm(name))
  }

  def viewWork(period: String, run: String, user: String) = Action {

    val ActionFuncType       = "do"
    val PresentationFuncType = "present"

    val userWorks = SubmissionManager.getUserWork(period, run, user)
    val (actionJsSeq, presentationJsSeq) = userWorks.map(_.typ).distinct.map {
      name =>
        val bundle               = SubmissionManager.getTypeBundleByName(name)
        val actionFuncBody       = bundle map (_.actionJS)       getOrElse (generateDefaultJS(name, ActionFuncType))
        val presentationFuncBody = bundle map (_.presentationJS) getOrElse (generateDefaultJS(name, PresentationFuncType))
        (finalizeJS(actionFuncBody, name, ActionFuncType), finalizeJS(presentationFuncBody, name, PresentationFuncType))
    }.unzip

    Ok(views.html.submissions(userWorks, actionJsSeq.mkString, presentationJsSeq.mkString))

  }

  def updateAndViewWork(period: String, run: String, user: String) = Action {
    (submit(_: Request[AnyContent], UserWorkComment.fromMap(_), noCleanup)) andThen {
      case x if (x.header.status == OK) => Redirect(routes.Submission.viewWork(period, run, user))
      case x                            => x
    }
  }

  private def finalizeJS(funcBody: String, name: String, funcType: String) : String =
    """
      function %s_custom_%s(data) {
        %s
      }
    """.format(funcType, name, funcBody)

  private def generateDefaultJS(name: String, funcType: String) : String =
    """alert("No '%s' action defined for content type '%s'");""".format(funcType, name)

  //@ Kinda messy, but... about as good as it's going to get.  Maybe I could unify this data-replacement code...? --JAB
  //@ Detect the error here on `getTypeBundle... == None`
  def submitWork = APIAction {
    implicit request =>
      val fileRegistrationFunc = (workAndValidation: (UserWork, Validation[String, Long])) => {
        val (work, validation) = workAndValidation
        validation map { case id =>
          SubmissionManager.getTypeBundleByName(work.typ) map {
            bundle =>
              val newData = SubmissionFileManager.registerFile(work.data, id.toString, bundle)
              SubmissionManager.update(work.cloneWith(id = Option(id), data = newData))
          }
          id
        }
      }
      submit(request, UserWork.fromMap(_), fileRegistrationFunc)
  }

  def submitComment = APIAction {
    implicit request =>
      submit(request, UserWorkComment.fromMap(_), noCleanup)
  }

  def submitSupplement = APIAction {
    implicit request =>
      val fileRegistrationFunc = (supplementAndValidation: (UserWorkSupplement, Validation[String, Long])) => {
        val (supplement, validation) = supplementAndValidation
        validation map { case id =>
          SubmissionManager.getTypeBundleByName(supplement.typ) map {
            bundle =>
              val newData = SubmissionFileManager.registerFile(supplement.data, id.toString, bundle)
              SubmissionManager.update(supplement.cloneWith(id = Option(id), data = newData))
          }
          id
        }
      }
      submit(request, UserWorkSupplement.fromMap(_), fileRegistrationFunc)
  }

  private def submit[T <% Submittable](request: Request[AnyContent],
                                       constructorFunc: (Map[String, String]) => Validation[String, T],
                                       cleanup: ((T, Validation[String, Long])) => Validation[String, Long]) : SimpleResult[_] = {
    val params = PlayUtil.extractParamMapOpt(request) getOrElse Map() map { case (k, v) => (k, v(0)) }
    constructorFunc(params) flatMap {
      submittable =>
        val result = SubmissionManager.submit(submittable)
        cleanup(submittable, Success(result))
    } fold ((ExpectationFailed(_)), (x => Ok(x.toString)))
  }

}
