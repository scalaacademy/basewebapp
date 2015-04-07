package code.pages.theme

import com.github.david04.liftutils.loc.Loc
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, _}
import net.liftweb.util.Helpers._
import org.apache.commons.lang.StringEscapeUtils

import scala.xml.NodeSeq


trait BWANotifications extends BWAModals {

  def confirm(
               question: String,
               cancel: JsCmd = Noop,
               okLbl: String = "Ok",
               okClass: BtnClas = Btn.Success
               )(okCmd: => JsCmd) = new SaveCancelModal {


    override def saved(): JsCmd = hide() & okCmd
    override def n: String = "confirmModal"
    override def saveBtn: BtnClas = okClass
    override def saveBtnText: String = okLbl
    override def title: String = "Confirm"
    override def contents: NodeSeq = <h5><b>{question}</b></h5>
    override protected def p: Loc = null
  }.show()

  def notifyUser(title: String, text: String) = Run( s"""$$container.notify("create", "default", { title: ${
    title.encJs
  }, text: ${
    text.encJs
  } });""")

  def notifyUser(title: String, text: String, icon: TH.IcnFA.FAIcn, color: String = "green") =
    Run( s"""$$container.notify("create", "withIcon", {
            |  title: ${StringEscapeUtils.escapeHtml(title).encJs},
            |  text: ${StringEscapeUtils.escapeHtml(text).encJs},
            |  icon: '<span style="top:0;margin-top:-7px;" class=""><i class="${icon.cls}" style="color: $color;font-size: 40px;padding: 2px 4px 2px 5px;"></i></span>'
            |});""".stripMargin)

  def notifyUser(title: String, text: String, icon: TH.IcnBig.BigIcn) =
    Run( s"""$$container.notify("create", "withIcon", {
            |  title: ${StringEscapeUtils.escapeHtml(title).encJs},
            |  text: ${StringEscapeUtils.escapeHtml(text).encJs},
            |  icon: '<span style="top:0;margin-top:-7px;" class="${icon.cls}"></span>'
            |});""".stripMargin)

  def notifyUserSticky(title: String, text: String) =
    Run( s"""$$container.notify("create", "sticky", {
            |  title: ${StringEscapeUtils.escapeHtml(title).encJs},
            |  text: ${StringEscapeUtils.escapeHtml(text).encJs}
            |}, {expires: false});""".stripMargin)
}
