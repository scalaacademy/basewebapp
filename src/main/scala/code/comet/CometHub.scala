package code.comet

import net.liftweb.actor.LiftActor
import net.liftweb.http._


object CometHub extends LiftActor with ListenerManager {

  trait HubMessage

  object Initial extends HubMessage

  object UpdateQueuedActions extends HubMessage

  def createUpdate = Initial

  override def highPriority = {
    case m: HubMessage => updateListeners(m)
  }
}
