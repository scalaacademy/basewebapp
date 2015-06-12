package code.pages


import net.liftweb.http.SHtml
import net.liftweb.util.True

import scala.xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._

class DashboardPage extends MenuPage {

  override def pageTitle: String = "Dashboard"
  override def pageSubtitle: String = "BasicWebApp Sample Application"

  lazy val exampleModal: TH.Modal = new TH.Modal {
    override def modalTitle: String = " Example Modal"

    override def modelType: TH.ModalType.Value = TH.ModalType.Success

    override def modalContent: NodeSeq = (
      <button type="button" class={modelType.buttonClass} onclick={exampleModal.hide().toJsCmd}>Hide</button>)
  }

  override def render: (NodeSeq) => NodeSeq = super.render andThen (
    "#page-content *" #> {

      TH.Row(
        TH.Col.md12 {
          <button type="button" class="btn btn-primary" onclick={SHtml.ajaxInvoke(() => exampleModal.show()).toJsCmd}>Open Modal</button><br/><br/>
        }
      ) ++
        TH.Row(
          TH.Col.md4.md_offset2.sm6 {
            exampleWidget.renderedWidget
          },
          TH.Col.md4.sm6 {
            new TH.Widget {
              override def widgetType: TH.WidgetType.Value = TH.WidgetType.Info
              override def toTitle: String = "Hello World"
              override def contents: NodeSeq = Text(" Input something")
              override def solid: Boolean = true
            }.renderedWidget
          }
        )
    })

  lazy val exampleWidget: TH.Widget = new TH.Widget {
    override def widgetType: TH.WidgetType.Value = TH.WidgetType.Success

    override def toTitle: String = "TITLE"

    override def contents: NodeSeq = { scala.xml.Text(" Input something") }

    override def toCollapse: Boolean = true
    override def toRemove: Boolean = true

    override def solid: Boolean = true
  }
}