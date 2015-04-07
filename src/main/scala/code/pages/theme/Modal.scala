package code.pages.theme

import com.github.david04.liftutils.elem.ID
import com.github.david04.liftutils.loc.{LocC, LocP}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{SHtml, Templates}
import net.liftweb.util.BasicTypesHelpers
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait Modal extends ID with LocC {

  def n = "modal"
  def templateLoc = "templates-hidden" :: "modal" :: Nil

  val BIG = "big"
  val HUGE = "huge"
  val size = ""
  val titleId = id('title)

  def modalName = locPrefix
  def modalFullName = fullPrefix.head

  def setTitle(title: String) = Run(sel('title, ".text(" + title.encJs + ");"))

  def onCloseClientSide() = Noop

  def templateTransforms: NodeSeq => NodeSeq =
    ".modal [modal-name]" #> modalName &
      ".modal [modal-full-name]" #> modalFullName &
      ".modal [id]" #> modalId &
      ".modal [class+]" #> size &
      ".mdl-header [id]" #> titleId &
      ".mdl-header *" #> title &
      ".mdl-body *" #> contents &
      ".mdl-close [onclick]" #> onCloseClientSide()

  lazy val template = templateTransforms.apply(Templates(templateLoc).get)

  def contents: NodeSeq

  lazy val modalId = BasicTypesHelpers.randomString(10)

  def show() = Run("$(" + template.toString.encJs + ").appendTo('body');") & Run("$('#" + modalId + "').modal({backdrop: 'static',keyboard: false}).modal('show');")

  def hide() = Run("$('#" + modalId + "').modal('hide');")

  def title: String
}

trait SaveCancelModal extends Modal with ID {

  def defaultBtnClass = "btn-default"
  def defaultCancelBtnClass = "btn-default"
  def changedBtnClass = "btn-success"
  def saveBtnText = "Save"
  def cancelBtnText = "Cancel"
  override def templateLoc = "templates-hidden" :: "modal-save-cancel" :: Nil

  def saved(): JsCmd = Noop

  def savedClientSide(): JsCmd = Noop

  def cancelledClientSide(): JsCmd = Noop

  def changed(): JsCmd = Run(sel('save, s".removeClass('$defaultBtnClass').addClass('$changedBtnClass')"))

  override def templateTransforms: NodeSeq => NodeSeq =
    super.templateTransforms andThen
      ".mdl-cancel *" #> cancelBtnText &
        ".mdl-save *" #> saveBtnText &
        ".mdl-cancel [class+]" #> defaultCancelBtnClass &
        ".mdl-cancel [onclick]" #> cancelledClientSide().toJsCmd &
        ".mdl-save [class+]" #> defaultBtnClass &
        ".mdl-save [id]" #> id('save) &
        ".mdl-save [onclick]" #> (Run(sel('save, s".removeClass('$changedBtnClass').addClass('$defaultBtnClass')")) & savedClientSide() & SHtml.ajaxInvoke(() => saved()))
}

case class DefaultModal(val title: String, val large: Boolean, protected val p: LocP)(val contents: NodeSeq) extends Modal

//trait ModalEditor extends Modal with DefaultBS2HTMLEditor {
//
//  def saveBtnText = "Save"
//  override def templateLoc = "templates-hidden" :: "modal-editor" :: Nil
//
//  override def templateTransforms: NodeSeq => NodeSeq =
//    super.templateTransforms andThen
//      ".mdl-body *" #> renderedNoBtns &
//        ".mdl-save" #> submitBtnTransforms
//}