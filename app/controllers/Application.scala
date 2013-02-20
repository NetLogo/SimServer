package controllers

import play.api.mvc.{ Action, Controller }

import models.util.{ PlayUtil, Util }

object Application extends Controller {

  /*
   "Wow!  WHAT IS THIS?!", you ask.  Well, let me tell you...
   First, look into the HTTP request type OPTIONS.  Then, understand that this simply replies to the OPTIONS request,
   saying that we'll accept pretty much any request--cross-domain, or not
   */
  def options(nonsense: String) = APIAction { Ok }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def displayHttpRequest = APIAction {
    request =>
      val bundle = PlayUtil.extractBundle(request)
      val text = "\nRequest Type: \n" + request.method +
                 "\n\nHeaders: \n" + (request.headers.toSimpleMap map { case (k, v) => "%s: %s".format(k, v) } mkString("\n")) +
                 "\n\nBody: \n" + (
                   Util.noneIfEmpty(bundle.stringParams, ((_: Map[String, String]) map { case (k, v) => s"$k=$v" } mkString ("\n"))) getOrElse "[empty]"
                 ) + "\n\n" + (
                   bundle.byteParams map { case (k, v) => s"$k={{{${new String(v)}}}}" } mkString ("\n")
                 )
      Ok(text)
  }

}

