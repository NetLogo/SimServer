package models

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:40 PM
 */

sealed trait TempGenMessage
case class  Write(contents: String) extends TempGenMessage
case object Delete extends TempGenMessage

sealed trait HubNetMessage
case class Start(modelName: String) extends HubNetMessage

sealed trait ServerStatus
case object Started extends ServerStatus
case object InUse extends ServerStatus
