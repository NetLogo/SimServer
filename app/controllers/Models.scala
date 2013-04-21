package controllers

import
  play.api.{ libs, mvc },
    libs.json.Json,
    mvc.{ Controller, RequestHeader }

import
  controllers.action.APIAction

import
  models.util.{ NetUtil, ModelMapper }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 6/13/12
 * Time: 3:43 PM
 */

object Models extends Controller {

  private val PlainType = "plain"
  private val JsonType  = "json"

  def modelNames(responseType: String) = APIAction {
    val names = ModelMapper.modelNames
    responseType match {
      case PlainType => Ok(names.mkString("", "\n", "\n"))
      case JsonType  => import Json.toJson; Ok(toJson(names map (toJson(_))) + "\n")
      case x         => BadRequest("Unrecognized response type requested: " + x + "\n")
    }
  }

  protected[controllers] def getHubNetModelURL(modelName: String)(implicit request: RequestHeader) : String = {
    val name = urlify(ModelMapper.unalias(modelName))
    val ModelAssetURLFormat = "models/hubnet/%s.nlogo".format((_: String))
    routes.Assets.at(ModelAssetURLFormat(name)).absoluteURL(false)
  }

  // Takes model names and converts them to something that can be referenced as a URL
  private def urlify(name: String) : String = NetUtil.encodeForURL(name)

}
