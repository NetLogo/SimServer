package models

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:40 PM
 */

sealed trait FileActorMessage
case object Get                          extends FileActorMessage
case object Delete                       extends FileActorMessage
case object Initialize                   extends FileActorMessage
case class  Write(contents: Array[Byte]) extends FileActorMessage
