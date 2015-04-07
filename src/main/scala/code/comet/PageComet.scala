package code.comet

import code.util.DevMail
import net.liftweb
import net.liftweb.actor.LiftActor
import net.liftweb.http._
import net.liftweb.http.js.{JsCmd, JsCmds}

import scala.xml.NodeSeq


object PageCometHub extends LiftActor with ListenerManager {

  def createUpdate = null

  import PageComet._

  override def highPriority = {
    case Broadcast(mesg) => sendListenersMessage(Broadcast(mesg))
    case m: RunInPage => sendListenersMessage(m)
  }
}


trait CometMesg

object PageComet {

  case class PageRef(page: AnyRef) extends CometMesg

  case class Run(f: () => JsCmd) extends CometMesg

  case class RunInPage(mesg: PartialFunction[AnyRef, JsCmd], page: PageComet) extends CometMesg

  case class RunIn(f: () => JsCmd, millis: Long) extends CometMesg

  case class Broadcast(mesg: PartialFunction[AnyRef, JsCmd]) extends CometMesg

  case class InAllPages(mesg: PartialFunction[AnyRef, JsCmd]) extends CometMesg

}

class PageComet extends CometActor with CometListener {

  def registerWith = PageCometHub

  override def render: RenderOut = NodeSeq.Empty

  var page: AnyRef = _

  import PageComet._

  override def highPriority: PartialFunction[Any, Unit] = {
    case PageRef(p) =>
      println(name.getOrElse("Unnamed") + ": Got page ref")
      page = p

    case Run(f) =>
      val update = try {f()} catch {case e: Exception => {DevMail.ex(e); JsCmds.Noop}}
      if (update != JsCmds.Noop) {
        partialUpdate(update)
      }

    case RunInPage(mesg, pageComet) =>
      (try {mesg.lift(page)} catch {case e: Exception => {DevMail.ex(e); None}})
        .foreach(cmd => pageComet ! Run(() => cmd))

    case RunIn(f, millis) => liftweb.util.Schedule.schedule(this, Run(f), net.liftweb.util.Helpers.TimeSpan(millis))

    case Broadcast(mesg) =>
      (try {mesg.lift(page)} catch {case e: Exception => {DevMail.ex(e); None}})
        .foreach(cmd => partialUpdate(cmd))

    case InAllPages(mesg) => PageCometHub ! RunInPage(mesg, this)
  }
}
