package models

import java.net.URI

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 3:40 PM
 */

case class JNLPProps(
                     modelName: Option[String],
                     username: Option[String],
                     hubnetServerURI: Option[String],
                     hubnetServerPort: Option[Int]
                    )
