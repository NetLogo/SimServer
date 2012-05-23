package models.hubnet

import akka.actor.Actor
import models.{Start, Started}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/23/12
 * Time: 2:19 PM
 */

class HubNetServerActor extends Actor {
  protected def receive = {
    case Start(modelName) => /*start;*/ sender ! Started //@ Fix eventually
  }
}
