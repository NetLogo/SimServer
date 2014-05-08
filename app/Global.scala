import
  controllers.filter.LoggingFilter

import
  play.api.{ Application => PlayApp, mvc },
    mvc.WithFilters

object Global extends WithFilters(LoggingFilter) {
  override def onStart(app: PlayApp) : Unit = {
    controllers.Application.init()
  }
}
