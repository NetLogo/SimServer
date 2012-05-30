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
case object Initialize extends TempGenMessage

sealed trait HubNetMessage
case class Start(modelName: String, portNum: Int) extends HubNetMessage
case class Run(instance: AnyRef) //@ Change this to use `HeadlessWorkspace`, if we ever go forward with headless HubNet here...
case object End extends HubNetMessage

sealed trait ServerStatus
case object Started extends ServerStatus
case object InUse extends ServerStatus
