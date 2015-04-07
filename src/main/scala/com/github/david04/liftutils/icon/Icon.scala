package com.github.david04.liftutils.icon

import com.github.david04.liftutils.elem._
import com.github.david04.liftutils.fontawesome
import com.github.david04.liftutils.fontawesome.Icon
import com.github.david04.liftutils.loc.{LocC, LocP}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

trait IconElem extends HTMLEditableElem with LabeledElem {
  implicit def self = this

  type Icon

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-icon-dflt" :: Nil

  def getIcon: () => Option[Icon]

  def allIcons: () => Seq[Icon]

  def icon2NodeSeq: (Icon, JsCmd) => NodeSeq

  def setIcon: Option[Icon] => JsCmd

  var value = getIcon()

  def save(): JsCmd = setIcon(getCurrentValue())

  def getCurrentValue(): Option[Icon] = value

  override protected def htmlElemRendererTransforms = {
    val rows = allIcons().grouped(6).toSeq
    ".edit-icon-around" #> SHtml.idMemoize(renderer =>
      ".edit-icon-row" #> rows.map(row =>
        ".edit-icon-cell" #> row.map(icon =>
          ".edit-icon-cell [class+]" #> (if (value == Some(icon)) "selected" else "") &
            ".edit-icon-cell *" #>
              icon2NodeSeq(icon, SHtml.ajaxInvoke(() => {
                value = (if (value == Some(icon)) Option.empty[Icon] else Some(icon))
                renderer.setHtml() & onChangeClientSide()
              }).cmd))))
  }
}

class IconModal(
                 val elemName: String,
                 get: => Option[Icon],
                 set: Option[Icon] => Unit,
                 all: Seq[Icon],
                 val enabled: () => Boolean = () => true
                 )(implicit protected val editor: DefaultHTMLEditor, protected val p: LocP) extends ModalEditElem with IconElem with EditableElem2DefaultEditorBridge with LocC {

  override def modalTransforms =
    super.modalTransforms andThen
      ".modal-dialog [class+]" #> "modal-wide"

  type Icon = fontawesome.Icon

  def getIcon = () => get

  def allIcons = () => all

  def icon2NodeSeq = (icon: Icon, onClick: JsCmd) => <span onclick={onClick.toJsCmd}><span class="icon">{icon.nodeSeq}</span> {icon.shortName}</span>

  def setIcon = set(_)

  def getCurrentViewString = getCurrentValue().map(_.name).getOrElse(loc("none"))

  override def onChangeClientSide(): JsCmd =
    super.onChangeClientSide() & setCurrentViewString(getCurrentValue().map(_.name).getOrElse(labelStr("none")))

}
