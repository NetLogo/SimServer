package controllers

import
  java.io.File

import
  play.api.mvc.{ Action, Controller }

object LameAssets extends Controller {

  def at(filepath: String) = Action {
    val file = new File(s"public/$filepath")
    if (file.exists())
      Ok.sendFile(content = file, inline = true)
    else
      NotFound
  }

}
