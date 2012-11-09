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
    implicit request =>
      val params = PlayUtil.commonExtractMap(request)
      val bundle = TypeBundle(params("name"), "", "", "") //@ Validate better
      SubmissionManager.submit(bundle)
      Redirect(routes.Submission.viewTypeEditForm(bundle.name))
  }

  def viewTypeEditForm(name: String) = Action {
    implicit request =>
      val bundleOpt = SubmissionManager.getTypeBundleByName(name)
      Ok(views.html.edit_sub_type(bundleOpt.get)) //@ Validate better
  }

  def editType(name: String) = Action {
    implicit request =>
      val params = PlayUtil.commonExtractMap(request)
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

  //@ Detect the error here on `getTypeBundle... == None`
  def submitWork = APIAction {
    implicit request =>
      val fileRegistrationFunc = registerFile[UserWork](_.typ)(_.data) {
        (id, newData) => _.cloneWith(id = Option(id), data = newData)
      } _
      submit(request, UserWork.fromMap(_), fileRegistrationFunc)
  }

  def submitComment = APIAction {
    implicit request =>
      submit(request, UserWorkComment.fromMap(_), noCleanup)
  }

  def submitSupplement = APIAction {
    implicit request =>
      val fileRegistrationFunc = registerFile[UserWorkSupplement](_.typ)(_.data) {
        (id, newData) => _.cloneWith(id = Option(id), data = newData)
      } _
      submit(request, UserWorkSupplement.fromMap(_), fileRegistrationFunc)
  }


  private def registerFile[T <% Updatable](getTypeNameFunc:     T => String)
                                          (getFileContentsFunc: T => String)
                                          (cloneFunc:           (Long, String) => T => T)
                                          (subjectAndStatus:   (T, Validation[String, Long])) : Validation[String, Long] = {
    val (subject, status) = subjectAndStatus
    status map { case id =>
      SubmissionManager.getTypeBundleByName(getTypeNameFunc(subject)) map {
        typeBundle =>
          val newData = SubmissionFileManager.registerFile(getFileContentsFunc(subject), id.toString, typeBundle)
          SubmissionManager.update(cloneFunc(id, newData)(subject))
      }
      id
    }
  }

  private def submit[T <% Submittable](request: Request[AnyContent],
                                       constructorFunc: (Map[String, String]) => Validation[String, T],
                                       cleanup: ((T, Validation[String, Long])) => Validation[String, Long]) : SimpleResult[_] = {
    val params = PlayUtil.commonExtractMap(request)
    constructorFunc(params) flatMap {
      submittable =>
        val result = SubmissionManager.submit(submittable) //@ Validate this; should not auto-`Success` on next line
        cleanup(submittable, Success(result))
    } fold ((ExpectationFailed(_)), (x => Ok(x.toString)))
  }

}
