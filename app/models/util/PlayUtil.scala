package models.util

import play.api.mvc.{ AnyContent, Request }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/5/12
 * Time: 5:03 PM
 */

object PlayUtil {
  def extractParamMapOpt(request: Request[AnyContent]) =
    request.body.asMultipartFormData.map(_.asFormUrlEncoded).
            orElse(request.body.asFormUrlEncoded flatMap (Util.noneIfEmpty)).
            orElse(Option(request.queryString))
}
