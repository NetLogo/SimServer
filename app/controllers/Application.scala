package controllers

import play.api.mvc._
import scalaz.{Success, Failure}
import models.filemanager.PermFileManager

object Application extends Controller {

  private val ExportKey      = "netlogo_export"
  private val PeriodIDKey    = "period_id"
  private val RunIDKey       = "run_id"
  private val WorkgroupIDKey = "workgroup_id"

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def displayHttpRequest = Action {
    request =>
    val text = "\nRequest Type: \n" + request.method +
               "\n\nHeaders: \n" + (request.headers.toSimpleMap map { case (k, v) => "%s: %s".format(k, v) } mkString("\n")) +
               "\n\nBody: \n" + ((request.body.asFormUrlEncoded flatMap {
                                    case x if x.isEmpty => None
                                    case x              => Some(x map { case (k, v) => "%s=%s".format(k, v(0)) } mkString ("\n"))
                                }) getOrElse ("[empty]")) + "\n\n"
    Ok(text)
  }

  def handleNetLogoExportWorld = Action {
    request =>
    val paramMap = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                           orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                           orElse(Option(request.queryString)) getOrElse Map() map { case (k, v) => (k, v(0)) }
    val status = (for {
      input    <- paramMap.get(ExportKey);
      periodID <- paramMap.get(PeriodIDKey);
      runID    <- paramMap.get(RunIDKey);
      userID   <- paramMap.get(WorkgroupIDKey)
    } yield {
      try Success(PermFileManager.registerFile(input, "%s_%s_%s_%s".format(System.currentTimeMillis(), periodID, runID, userID), "csv"))
      catch { case ex => Failure("Failed to write data: " + ex.getMessage) }
    }) getOrElse (Failure("Invalid POST data"))
    status.fold((ExpectationFailed(_)), (_ => Ok))
  }

}
