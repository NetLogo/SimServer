package controllers

import play.api.mvc.{ Action, Controller, RequestHeader }
import models.util.{ NetUtil, ModelMapper }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/13/12
 * Time: 3:43 PM
 */

//@ The proliferation of controllers in the 'controllers' package is getting a bit out of hand
//  I feel like I should do something about this; this architecture kinda sucks.... --JAB (8/29/12)
object Models extends Controller {

  def modelNames = Action { Ok(ModelMapper.modelNames mkString("", "\n", "\n")) }

  protected[controllers] def getHubNetModelURL(modelName: String)(implicit request: RequestHeader) : String = {
    val name = urlify(ModelMapper.unalias(modelName))
    val ModelAssetURLFormat = "misc/models/hubnet/%s.nlogo".format((_: String))
    routes.Assets.at(ModelAssetURLFormat(name)).absoluteURL(false)
  }

  // Takes model names and converts them to something that can be referenced as a URL
  private def urlify(name: String) : String = NetUtil.encodeForURL(name)

}
