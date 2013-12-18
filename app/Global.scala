import
  play.api.{ Application => PlayApp, GlobalSettings }

object Global extends GlobalSettings {
  override def onStart(app: PlayApp) : Unit = {
    controllers.Application.init()
  }
}
