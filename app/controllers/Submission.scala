package controllers

import
  scalaz.{ NonEmptyList, Scalaz, ValidationNel },
    Scalaz._

import
  play.api.mvc.{ Action, AnyContent, Controller, Request, SimpleResult }

import
  models.{ submission, util },
    submission._,
    util.{ ParamBundle, PlayUtil }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:54 PM
 */

object Submission extends Controller {

  protected class EnhancedParamMap(paramMap: Map[String, String]) {
    def extract(key: String) : ValidationNel[String, String] =
      paramMap.get(key) map (_.successNel[String]) getOrElse s"No such parameter found: $key".failNel
  }

  implicit def paramMap2Enhanced(paramMap: Map[String, String]) = new EnhancedParamMap(paramMap)

  private val noCleanup = ((_: (_, Long))._2.successNel[String])

  private def nel2Str(nel: NonEmptyList[String]) = nel.list.mkString("\n")

  // These three seem to be a bit of a misfit, but... it actually kind of makes sense to do them here, when you think about it
  def listRuns = APIAction {
    Ok(SubmissionDBManager.getRuns.mkString("\n"))
  }

  def listPeriodsIn(runID: String) = APIAction {
    Ok(SubmissionDBManager.getPeriodsByRun(runID).mkString("\n"))
  }

  def listStudentsIn(runID: String, periodID: String) = APIAction {
    Ok(SubmissionDBManager.getStudentsByRunAndPeriod(runID, periodID).mkString("\n"))
  }

  def viewTypeCreationForm = Action {
    Ok(views.html.create_sub_type())
  }

  def createType = Action {
    implicit request =>
      val params    = PlayUtil.commonExtractMap(request)
      val nameMaybe = params extract "name"
      nameMaybe flatMap {
        name =>
          val bundle = TypeBundle(name, "", "", "")
          SubmissionDBManager.submit(bundle) map { _ => bundle }
      } fold (
        (nel    => ExpectationFailed(nel2Str(nel))),
        (bundle => Redirect(routes.Submission.viewTypeEditForm(bundle.name)))
      )
  }

  def viewTypeEditForm(name: String) = Action {
    implicit request =>
      SubmissionDBManager.getTypeBundleByName(name) fold (
        (nel => ExpectationFailed(nel2Str(nel))), (bundle => Ok(views.html.edit_sub_type(bundle)))
      )
  }

  def editType(name: String) = Action {
    implicit request =>
      val params            = PlayUtil.commonExtractMap(request)
      val actionMaybe       = params extract "action_js"
      val presentationMaybe = params extract "presentation_js"
      val fileExtMaybe      = params extract "file_extension"
      (actionMaybe |@| presentationMaybe |@| fileExtMaybe) {
        (action, presentation, fileExt) =>
          val bundle = TypeBundle(name, action, presentation, fileExt)
          SubmissionDBManager.update(bundle)
      } fold (
        (nel => ExpectationFailed(nel2Str(nel))),
        (_   => Redirect(routes.Submission.viewTypeEditForm(name)))
      )
  }

  def viewWork1(run: String) = Action {
    presentWork(SubmissionDBManager.getUserWork(run))
  }

  def viewWork2(run: String, period: String) = Action {
    presentWork(SubmissionDBManager.getUserWork(run, period))
  }

  def viewWork3(run: String, period: String, user: String) = Action {
    presentWork(SubmissionDBManager.getUserWork(run, period, user))
  }

  private def presentWork(userWorks: Seq[UserWork]) : SimpleResult[_] = {

    val ActionFuncType       = "do"
    val PresentationFuncType = "present"

    val (actionJsSeq, presentationJsSeq) = userWorks.map(_.typ).distinct.map {
      name =>
        val bundle               = SubmissionDBManager.getTypeBundleByName(name)
        val actionFuncBody       = bundle map (_.actionJS)       getOrElse (generateDefaultJS(name, ActionFuncType))
        val presentationFuncBody = bundle map (_.presentationJS) getOrElse (generateDefaultJS(name, PresentationFuncType))
        (finalizeJS(actionFuncBody, name, ActionFuncType), finalizeJS(presentationFuncBody, name, PresentationFuncType))
    }.unzip

    Ok(views.html.submissions(userWorks, actionJsSeq.mkString, presentationJsSeq.mkString))

  }

  def updateAndViewWork(run: String, period: String, user: String) = Action {
    (submit(_: Request[AnyContent], UserWorkComment.fromBundle(_), noCleanup)) andThen {
      case x if (x.header.status == OK) => Redirect((period, user) match {
        case ("", "") => routes.Submission.viewWork1(run)
        case (p, "")  => routes.Submission.viewWork2(run, p)
        case (p, u)   => routes.Submission.viewWork3(run, p, u)
      })
      case x => x
    }
  }

  private def finalizeJS(funcBody: String, name: String, funcType: String) : String =
   s"""
      |function ${funcType}_custom_$name(data) {
      |  $funcBody
      |}
    """.stripMargin

  private def generateDefaultJS(name: String, funcType: String) : String =
    s"""alert("No '$funcType' action defined for content type '$name'");"""

  def submitWork = APIAction {
    implicit request =>
      val fileRegistrationFunc = registerFile[UserWork](_.typ)(_.rawData) {
        (id, newData) => _.copy(id = Option(id), data = newData)
      } _
      submit(request, UserWork.fromBundle(_), fileRegistrationFunc)
  }

  def submitSupplement = APIAction {
    implicit request =>
      val fileRegistrationFunc = registerFile[UserWorkSupplement](_.typ)(_.rawData) {
        (id, newData) => _.copy(id = Option(id), data = newData)
      } _
      submit(request, UserWorkSupplement.fromBundle(_), fileRegistrationFunc)
  }


  private def registerFile[T <% Updatable](getTypeNameFunc:     T => String)
                                          (getFileContentsFunc: T => Array[Byte])
                                          (cloneFunc:           (Long, String) => T => T)
                                          (subjectAndID:        (T, Long)) : ValidationNel[String, Long] = {
    val (subject, id) = subjectAndID
    SubmissionDBManager.getOrCreateTypeBundleByName(getTypeNameFunc(subject)) map {
      typeBundle =>
        val newData = SubmissionFileManager.registerFile(getFileContentsFunc(subject), id.toString, typeBundle)
        SubmissionDBManager.update(cloneFunc(id, newData)(subject))
        id
    }
  }

  private def submit[T <% Submittable](request: Request[AnyContent],
                                       constructorFunc: (ParamBundle) => ValidationNel[String, T],
                                       cleanup: ((T, Long)) => ValidationNel[String, Long]) : SimpleResult[_] = {
    val paramBundle = PlayUtil.extractBundle(request)
    constructorFunc(paramBundle) flatMap {
      submittable =>
        val submissionStatus = SubmissionDBManager.submit(submittable)
        submissionStatus flatMap (id => cleanup(submittable, id))
    } fold ((nel => ExpectationFailed(nel2Str(nel))), (x => Ok(x.toString)))
  }

}
