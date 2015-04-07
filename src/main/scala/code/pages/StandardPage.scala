package code.pages

import code.comet.{PageCometHub, PageComet}
import code.model._
import code.pages.theme.{Libs, Styles, TH}
import code.util.actors.ActorSystemFacade
import com.github.david04.liftutils.loc.LocP
import com.github.david04.liftutils.reactive3.{Listener, EmmiterStateful, Emmiter}
import net.liftweb.common.Full
import net.liftweb.http.js.{JE, JsExp, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml, SessionVar}
import net.liftweb.util.Helpers._
import net.liftweb.util.{Helpers, ClearNodes, PassThru}
import org.squeryl.PrimitiveTypeMode._

import scala.xml.NodeSeq

class AppState {
  val onNextPage = collection.mutable.ListBuffer[JsCmd]()
}

object StateVar extends SessionVar[AppState](new AppState)

trait StandardPage extends LocP {

  lazy val TH = code.pages.theme.TH
  lazy val simplePageName = getClass.getSimpleName
  val state = StateVar.is
  val thisHref = S.uriAndQueryString.getOrElse("/")

  val cometName = Helpers.nextFuncName
  S.session.foreach(_.sendCometActorMessage(classOf[PageComet].getSimpleName, Full(cometName), PageComet.PageRef(this)))

  def cometMesg(f: => JsCmd): Unit = S.session.foreach(_.sendCometActorMessage(classOf[PageComet].getSimpleName, Full(cometName), PageComet.Run(() => f)))

  def inComet(f: => JsCmd): JsCmd = { S.session.foreach(_.sendCometActorMessage(classOf[PageComet].getSimpleName, Full(cometName), PageComet.Run(() => f))); Noop }

  def inCometIn(millis: Long)(f: => JsCmd): Unit = S.session.foreach(_.sendCometActorMessage(classOf[PageComet].getSimpleName, Full(cometName), PageComet.RunIn(() => f, millis)))

  def broadcast(pf: PartialFunction[AnyRef, JsCmd]): Unit = PageCometHub ! PageComet.Broadcast(pf)

  def inInAllPages(pf: PartialFunction[AnyRef, JsCmd]): Unit = S.session.foreach(_.sendCometActorMessage(classOf[PageComet].getSimpleName, Full(cometName), PageComet.InAllPages(pf)))

  def libs: List[Libs.Lib] = List(
    //    Libs.jquery,
  )

  def libsInit: NodeSeq = libs.map(lib => <script src={lib} type="text/javascript"></script>).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty)

  def styles: List[Styles.Style] = List(
    //    Styles.bootstrap,
  )

  def stylesInit: NodeSeq = styles.map(style => <link href={style} rel="stylesheet" type="text/css"></link>).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty)

  lazy val sys = Sys()
  implicit lazy val user = User.currentUser
  lazy val props = user._props.in("page-" + locPrefix)
  lazy val acc = user.account

  lazy val userFirstNameEmmiter = new EmmiterStateful {override def initialValue(): JsExp = JE.Str(user.firstName)}
  lazy val userLastNameEmmiter = new EmmiterStateful {override def initialValue(): JsExp = JE.Str(user.lastName)}
  lazy val userEmailEmmiter = new EmmiterStateful {override def initialValue(): JsExp = JE.Str(user.email)}

  def shortcuts: List[(String, JsCmd)] = Nil

  def warnBeforeLeaving(mesg: Option[String]) = Run( s"""window.onbeforeunload = function(){return ${mesg.map(_.encJs).getOrElse("null")};};""")

  def initJs() = try {
    userFirstNameEmmiter.setUp() &
      userLastNameEmmiter.setUp() &
      userEmailEmmiter.setUp() &
      state.onNextPage.reduceOption[JsCmd](_ & _).getOrElse(Noop) &
      shortcuts.map({ case (key, func) => Run( s"""$$(document).bind('keydown', ${key.encJs}, function() {${func.toJsCmd}});""") }).reduceOption[JsCmd](_ & _).getOrElse(Noop)
  } finally {
    state.onNextPage.clear()
  }

  def extraHtml: NodeSeq = <lift:comet type={classOf[PageComet].getSimpleName} name={cometName}></lift:comet>

  def render: NodeSeq => NodeSeq =
    "body *+" #> {<tail>{Script(OnLoad(initJs()))}</tail> ++ libsInit ++ <head_merge>{stylesInit}</head_merge> ++ extraHtml} andThen
      "@fullname" #> <span>{Listener.span(userFirstNameEmmiter)} {Listener.span(userLastNameEmmiter)}</span> andThen
      "@email" #> Listener.span(userEmailEmmiter) andThen
      processLoc()

}
