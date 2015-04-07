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

import com.github.david04.liftutils.loc.LocC
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml2, Templates}
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

import scala.xml.NodeSeq

trait ID {
  private val _id = S.formFuncName

  def id(part: Symbol) = _id + "_" + part.name

  def sel(part: Symbol, func: String = "") = "$('#" + id(part) + "')" + func
}

trait Elem extends ID {}

trait ValidatableElem extends Elem {
  def error(): Option[NodeSeq] = None
}

trait ViewableElem extends Elem {}

trait FocusableElem extends Elem { def focusId: String}

trait NodeSeqViewableElem extends ViewableElem {def renderNodeSeqView: NodeSeq}

trait NamedElem extends ViewableElem {def elemName: String}

trait LabeledElem extends NamedElem with LocC {
  def n = elemName
  def labelStr: String = withPrefix("elemLbl").loc(elemName)
  def labelStrOpt(suffix: String): Option[String] = withPrefix("elemLbl").withPrefix(elemName).locOpt(suffix)
  def labelStr(suffix: String): String = withPrefix("elemLbl").withPrefix(elemName).loc(suffix)
  def glabelStr(suffix: String): String = withPrefix("elemLbl").loc(suffix)
}

trait EditableElem extends ViewableElem with ValidatableElem with NamedElem {

  protected def framework: Framework

  val enabled: () => Boolean

  def save(): JsCmd
}

trait UpdatableElem extends ViewableElem {

  def update(): JsCmd
}

trait HTMLViewableElem extends ViewableElem with NamedElem with UpdatableElem {

  def reloadValue(): JsCmd = Noop

  protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-view-dflt" :: Nil

  protected def htmlElemTemplate = Templates(htmlElemTemplatePath).get

  protected lazy val htmlElemRenderer = SHtml2.memoizeElem(_ => htmlElemRendererTransforms)

  protected def htmlElemRendererTransforms: NodeSeq => NodeSeq = PassThru

  def rerenderHtmlElem(): JsCmd = htmlElemRenderer.setHtml()

  def renderElem: NodeSeq = htmlElemRenderer.apply(htmlElemTemplate)

  protected def wrapName(name: String) = name + ": "

  def update(): JsCmd = Noop

  def requiresIFrameSubmit: Boolean = false
}

trait HTMLEditableElem extends HTMLViewableElem with EditableElem with LocC {

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-dflt" :: Nil

  def onChangeClientSide(): JsCmd = {
    if (error().isDefined) htmlEditableElemShowValidation = true
    else htmlEditableElemShowValidation = false
    update()
  }

  protected var htmlEditableElemShowValidation = false

  def onFailedSaveAttempt(): Unit = htmlEditableElemShowValidation = true
  def onSucessfulSave(): Unit = htmlEditableElemShowValidation = false

  def submit(): JsCmd = Noop

  def updateValidation(): JsCmd = error match {
    case Some(error) if htmlEditableElemShowValidation =>
      Run(
        sel('error) + ".html(" + error.toString.encJs + "); " +
          sel('wrapper) + ".addClass(" + framework.errorClass.encJs + "); ")
    case _ =>
      Run(
        sel('error) + ".html(''); " +
          sel('wrapper) + ".removeClass(" + framework.errorClass.encJs + "); ")
  }

  protected def enableDisableTransitionTime = 300

  import com.github.david04.liftutils.util.LUtils._

  override def update() =
    super.update() &
      (if (enabled())
        Run(sel('wrapper) + s".show();").P & updateValidation()
      else
        Run(sel('wrapper) + s".hide();").P)
  //  super.update() &
  //    (if (enabled())
  //      Run(sel('wrapper) + s".slideDown($enableDisableTransitionTime);").P & updateValidation().P
  //    else
  //      Run(sel('wrapper) + s".slideUp($enableDisableTransitionTime);")).P
}
