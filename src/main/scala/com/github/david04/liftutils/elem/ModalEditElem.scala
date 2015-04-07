//  Copyright (c) 2014 David Miguel Antunes <davidmiguel {at} antunes.net>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.

package com.github.david04.liftutils.elem

import com.github.david04.liftutils.modal.Modal
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.{SHtml, Templates}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait ModalEditElem extends HTMLEditableElem with Modal with LabeledElem {

  protected def htmlModalEditableElemViewTemplatePath: List[String] = "templates-hidden" :: "elem-modaledit-dflt-view" :: Nil

  protected lazy val htmlModalEditableElemViewTemplate = Templates(htmlModalEditableElemViewTemplatePath).get

  protected lazy val htmlModalEditableElemViewRenderer = SHtml.idMemoize(_ => htmlModalEditableElemViewRendererTransforms)

  protected def getCurrentViewString(): String
  protected def setCurrentViewString(s: String): JsCmd = Run("$('#" + id('vinput) + "').attr('value', " + s.encJs + ");")

  protected def htmlModalEditableElemViewRendererTransforms: NodeSeq => NodeSeq =
    ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
      ".elem-wrap [id]" #> id('vwrapper) &
      ".elem-lbl *" #> wrapName(labelStr) &
      ".elem-error [id]" #> id('verror) &
      ".elem-lbl *" #> wrapName(labelStr) &
      "input [value]" #> getCurrentViewString() &
      "input [id]" #> id('vinput) &
      ".edit-btn [onclick]" #> SHtml.onEvent(_ => {
        show()
      })

  protected def rerenderHtmlModalEditableElemView(): JsCmd = htmlModalEditableElemViewRenderer.setHtml()

  private[elem] def renderModalEditableElemView: NodeSeq = htmlModalEditableElemViewRenderer.apply(htmlModalEditableElemViewTemplate)

  override def renderElem: NodeSeq = renderModalEditableElemView

  protected def action: Option[(String, net.liftweb.http.js.JsCmd)] = Some((glabelStr("done"), hide()))
  protected def cancel: Option[(String, net.liftweb.http.js.JsCmd)] = None
  protected def content: Modal => scala.xml.NodeSeq = (_: Modal) => super.renderElem
  protected def height: Option[Int] = None
  protected def title: String = labelStr("modalTitle")

  override def update() =
    super.update() & {
      if (enabled())
        Run(sel('vwrapper) + ".fadeIn(300);") &
          (error.map(error => Run(
            sel('verror) + ".html(" + error.toString.encJs + "); " +
              sel('vwrapper) + ".addClass(" + framework.errorClass.encJs + "); "))
            .getOrElse(Run(
            sel('verror) + ".html(''); " +
              sel('vwrapper) + ".removeClass(" + framework.errorClass.encJs + "); ")))
      else
        Run(sel('vwrapper) + ".fadeOut();")
    }
}
