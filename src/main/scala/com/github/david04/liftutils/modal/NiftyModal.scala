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

package com.github.david04.liftutils.modal

import com.github.david04.liftutils.loc.{LocC, LocP}
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.NodeSeq

trait NiftyModal extends LocC {
  def n = "modal"
  protected lazy val id = S.formFuncName
  protected lazy val templateLoc = "templates-hidden" :: "nifty-modal" :: "modal-dflt" :: Nil
  protected lazy val template = modalTransforms.apply(Templates(templateLoc).get)

  protected def title: String
  protected def modalClasses: String = ""
  protected def content: NodeSeq
  protected def cancel: Option[(String, JsCmd)]
  protected def action: Option[(String, JsCmd)]

  def modalActionBtnTransforms: NodeSeq => NodeSeq =
    action.map({
      case (lbl, jsCmd) =>
        ".mdl-action *" #> lbl &
          ".mdl-action [onclick]" #> jsCmd
    }).getOrElse(".mdl-action" #> ClearNodes)

  def modalCancelBtnTransforms: NodeSeq => NodeSeq =
    cancel.map({
      case (lbl, jsCmd) =>
        ".mdl-cancel *" #> lbl &
          ".mdl-cancel [onclick]" #> jsCmd
    }).getOrElse(".mdl-cancel" #> ClearNodes)

  def modalTransforms: NodeSeq => NodeSeq =
    ".mdl [id]" #> id &
      ".mdl [class+]" #> modalClasses &
      ".mdl-title *" #> title &
      ".mdl-contents" #> content &
      ".mdl-cancel" #> modalCancelBtnTransforms andThen
      ".mdl-action" #> modalActionBtnTransforms

  def show() =
    Run(s"if(document.getElementById('$id') == null) " +
      "$(" + template.toString.encJs + ").appendTo('body');") &
      Run("$('#" + id + "').niftyModal('show');")

  def hide() = Run("$('#" + id + "').niftyModal('hide');")
}

case class DefaultNiftyModal(
                              protected val title: String,
                              protected val content: NodeSeq,
                              protected val cancelLbl: String,
                              protected val action: Option[(String, JsCmd)],
                              protected val height: Option[Int] = None)(implicit protected val p: LocP) extends NiftyModal {

  def cancel = Some((cancelLbl, hide()))
}
