package code.util.actors

import akka.actor.ActorSystem
import net.liftweb.common.Logger


object ActorSystemFacade extends Logger {

  var started = false

  case class Start()

  case class Stop()

  val system = ActorSystem("actors")

  def start() {
    if (!started) {
      started = true
      _logger.debug("Starting actor system")
    }
  }

  def stop() {
    if (started) {
      started = false
    }
  }
}

