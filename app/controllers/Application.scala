package controllers

import play.api._
import play.api.mvc._
import models.LoggingHandler

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def ws = Action {
    Ok(views.html.ws())
  }
  
  def startLogging = Action {
    Ok("/" + LoggingHandler.createNewLog())
  }
  
  def logData(id: String) = Action {
    request =>
      LoggingHandler.log(id.toLong, request.body.toString())
      Ok
  }
  
}
