package code.pages.theme

import com.github.david04.liftutils.elem.DefaultBS3HTMLEditor
import com.github.david04.liftutils.loc.LocC
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


abstract class ModalEditor(
                            protected val height: Option[Int] = None
                            ) extends com.github.david04.liftutils.modal.Modal with DefaultBS3HTMLEditor with LocC {

  protected lazy val title = loc("title")
  override def modalActionBtnTransforms: NodeSeq => NodeSeq =
    (".modal-action [class+]" #> "editor-btn-submit" &
      ".modal-action *" #> loc("lbl-action")) andThen
      submitBtnTransforms

  override protected def saved(): JsCmd = { super.saved() & hide() }

  override def cancel: Option[(String, JsCmd)] = Some((loc("lbl-cancel"), hide()))
  def action = None

  protected def content: com.github.david04.liftutils.modal.Modal => scala.xml.NodeSeq = (_: com.github.david04.liftutils.modal.Modal) => renderedNoBtns
}
