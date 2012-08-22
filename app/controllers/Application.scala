package controllers

import play.api.mvc._
import scalaz.{Success, Failure}
import models.filemanager.PermFileManager

object Application extends Controller {

  private val ExportKey   = "netlogo_export"
  private val PeriodIDKey = "period_id"
  private val RunIDKey    = "run_id"
  private val UserIDKey   = "user_id"

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def handleExport = Action {
    request =>
    val paramMap = request.body.asMultipartFormData.map(_.asFormUrlEncoded).
                           orElse(request.body.asFormUrlEncoded flatMap { case argMap => if (!argMap.isEmpty) Some(argMap) else None }).
                           orElse(Option(request.queryString)) getOrElse Map() map { case (k, v) => (k, v(0)) }
    val status = (for {
      input    <- paramMap.get(ExportKey);
      periodID <- paramMap.get(PeriodIDKey);
      runID    <- paramMap.get(RunIDKey);
      userID   <- paramMap.get(UserIDKey)
    } yield {
      try Success(PermFileManager.registerFile(input, "%s_%s_%s_%s".format(System.currentTimeMillis(), periodID, runID, userID), "csv"))
      catch { case ex => Failure("Failed to write data: " + ex.getMessage) }
    }) getOrElse (Failure("Invalid POST data"))
    status.fold((ExpectationFailed(_)), (_ => Ok))
  }

}
