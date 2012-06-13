package controllers

import play.api.mvc._
import models.util.ModelMap

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/13/12
 * Time: 3:43 PM
 */

object Models extends Controller {
  private val ModelAssetURLFormat = "misc/models/%s.nlogo".format((_: String))
  def getModelURL(modelName: String)(implicit request : play.api.mvc.RequestHeader) : String = {
    routes.Assets.at(ModelAssetURLFormat(ModelMap(modelName))).absoluteURL(false)
  }
}
