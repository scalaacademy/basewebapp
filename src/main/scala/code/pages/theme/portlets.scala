package code.pages.theme

import java.util.Date

import com.github.david04.liftutils.elem.ID
import com.github.david04.liftutils.loc.LocC
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.JsonAST.{JArray, JString, JValue}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait BWAPortlets extends BWAIcons with BWAButtons {

  trait Portlet extends ID {

    def portletAroundId = id('portletAround)
    def portletContainerId = id('portletContainer)
    def portletContentsId = id('portletContents)
    def portletActions: NodeSeq = NodeSeq.Empty
    def portletIcon: Option[Icn]
    def portletTitle: String
    def portletContents: NodeSeq
    def portletHeight: Option[Int] = None

    def portletWidgetBlock: Boolean = true

    def collapsible: Boolean = false

    var collapsed = false
    lazy val collapseButton = {
      val renderer = SHtml.idMemoize(renderer => (_: NodeSeq) => {
        <a onclick={Run( s"""$$('#$portletContainerId').slide${if (collapsed) "Down" else "Up"}(400)""") & SHtml.ajaxInvoke(() => {collapsed = !collapsed; renderer.setHtml()})}
           href="javascript:void(0)"
           class="btn"
           style="padding: 4px 8px;font-size: 16px;"
        ><span class={if (collapsed) "fa-angle-double-up" else "fa-angle-double-down"}></span></a>
      })
      renderer(<div class="btn-toolbar top-toolbar pull-right"></div>)
    }

    def rendered: NodeSeq = {
      <div id={portletAroundId} class="box-widget">
        <div class="widget-head clearfix">
          {portletIcon.map(icn => <span class="h-icon"><i class={s"gray-icons ${icn.cls}"}></i></span>).getOrElse(NodeSeq.Empty)}
          <h4>{portletTitle}</h4>
          {portletActions}
          {if (collapsible) collapseButton else NodeSeq.Empty}
        </div>
        <div id={portletContainerId} class="widget-container">
          <div style={portletHeight.map(h => s"max-height: ${h}px;overflow: auto;margin: 1px;").orNull}>
            <div id={portletContentsId} class={if (portletWidgetBlock) "widget-block" else null}>
              {portletContents}
            </div>
          </div>
        </div>
      </div>
    }

    def rerenderContents(): JsCmd = SetHtml(portletContentsId, portletContents)

    def rerenderAll(): JsCmd = Replace(portletAroundId, rendered)
  }

}
