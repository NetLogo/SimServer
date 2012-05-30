package models.hubnet

import akka.actor.{PoisonPill, Actor}
import models.{End, Run, Start, Started}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:19 PM
 */

class HubNetServerActor extends Actor {

  protected def receive = {

    case Start(modelName, port) =>

      val workspace = null //@ HeadlessWorkspace.newInstance(portNum)
      openModel(workspace, modelName)
      runCommand(workspace, "hubnet-reset")
      runCommand(workspace, "setup")

      sender ! Started

    case Run(workspace) =>
      runCommand(workspace, "go")
      self ! Run(workspace)

    //@ Not currently used; should probably happen after an hour or two of running
    case End =>
      self ! PoisonPill

  }

  //@ "AnyRef" will instead be "HeadlessWorkspace" if this ever moves forward
  private def openModel(workspace: AnyRef, modelName: String) {
    //@ Herp, derp, model-opening logic; exception handling
  }

  //@ "AnyRef" will instead be "HeadlessWorkspace" if this ever moves forward
  private def runCommand(workspace: AnyRef, cmd: String) {
    try {
      //@ workspace.command(cmd)
    }
    catch {
      case ex => ex.printStackTrace();
    }
  }

}
