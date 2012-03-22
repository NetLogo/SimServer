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

  //@ For testing only.  This has potential for trouble.
  // In the code that ends up being deployed, people should not be able to see other people's logs!
  def retrieveData(id: String) = Action {
    Ok(LoggingHandler.retrieveLogText(id.toLong))
  }
  
  def logData(id: String) = Action {
    request =>
      LoggingHandler.log(id.toLong, request.body.toString())
      Ok
  }
  
}
