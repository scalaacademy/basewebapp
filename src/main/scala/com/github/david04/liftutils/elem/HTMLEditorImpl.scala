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

import com.github.david04.liftutils.loc.{Loc, LocC}
import net.liftweb.http.{SHtml, _}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearNodes, Helpers, PassThru}

import scala.xml.NodeSeq


trait HTMLViewer extends ID {

  def locPrefix: String

}

trait HTMLEditor[E <: HTMLViewableElem] extends Loc {

  def framework: Framework

  def elemChanged(elem: E): JsCmd = Noop

  def submitForm(): JsCmd
}

trait BasicHTMLEditor extends HTMLEditor[HTMLViewableElem]

trait HTMLEditorImpl extends HTMLViewer with BasicHTMLEditor {

  implicit lazy val editor: this.type = this

  protected var modified: Boolean = false
  protected var saveFailed: Boolean = false

  protected def buildElems(): Seq[HTMLViewableElem]

  protected def editorAllTemplatePath = "templates-hidden" :: "editor-all" :: Nil

  protected def editorAllTemplate = processLoc()(Templates(editorAllTemplatePath).get)

  protected lazy val viewableElems = buildElems()
  protected def elems = viewableElems.collect({ case e: HTMLEditableElem => e})

  protected def isValid: Boolean = fieldError().isEmpty

  protected def fieldError(): Option[NodeSeq] = elems.filter(_.enabled()).flatMap(_.error).headOption

  protected def onFailedSaveAttempt(): Unit = {
    elems.foreach({ case e: HTMLEditableElem => e.onFailedSaveAttempt()})
  }

  protected def onSucessfulSave(): Unit = {
    elems.foreach({ case e: HTMLEditableElem => e.onSucessfulSave()})
  }

  protected def update(): JsCmd =
    elems.foldLeft(Noop)(_ & _.update()) &
      elems.foldLeft(Noop)(_ & _.update()) &
      elems.foldLeft(Noop)(_ & _.update())

  protected def saveDisabledElems = true

  protected def onSubmit() = {
    if (!isValid) {
      saveFailed = true
      onFailedSaveAttempt()
      update()
    } else {
      onSucessfulSave()
      elems.filter(e => saveDisabledElems || e.enabled()).foldLeft(Noop)(_ & _.save()) & savedInternal() & update()
    }
  }

  def submitBtnLabel = "Save"

  def submitBtnTransforms: NodeSeq => NodeSeq =
    ".editor-btn-submit *" #> submitBtnLabel &
      ".editor-btn-submit [onclick]" #> submitForm() &
      ".editor-btn-submit [id]" #> submitBtnId

  protected val iframeId = Helpers.nextFuncName

  lazy val submitBtnRenderer = SHtml2.memoizeElem(_ => submitBtnTransforms)
  def submitBtnId = submitBtnRenderer.latestId

  lazy val submitFuncName = S.formFuncName
  S.formGroup(1)(S.addFunctionMap(submitFuncName, (() => onSubmit())))

  def submitForm(): JsCmd = Run(
    (if (requiresIFrameSubmit) "" else "liftAjax.lift_uriSuffix = '" + submitFuncName + "=_';") +
      "$('#" + id('form) + "').submit();"
  )

  lazy val requiresIFrameSubmit: Boolean = viewableElems.exists(_.requiresIFrameSubmit)

  protected def editorTransforms() = {
    ".editor-btn-submit" #> submitBtnRenderer andThen
      ".editor-elems" #> ((_: NodeSeq) => viewableElems.map(elem => <div class={s"editor-elem-${elem.elemName}"}></div>)) andThen
      viewableElems.map(elem => s".editor-elem-${elem.elemName}" #> ((_: NodeSeq) => elem.renderElem)).reduceOption(_ & _).getOrElse(PassThru) andThen
      ".editor-btn-lbl *" #> loc("submitBtn") andThen ({
      if (!requiresIFrameSubmit) PassThru
      else (ns: NodeSeq) => {
        ns ++ Script(OnLoad(Run(
          s"""
             |${sel('form)}
             |.removeAttr('onsubmit')
             |.attr('action', '/ajax_request/' + lift_page)
             |.attr('method', 'post')
             |.attr('target', '$iframeId')
             |.attr('enctype', 'multipart/form-data' )
             |.attr('encoding', 'multipart/form-data')
             |.find('input:submit,button[type=submit]')
             |.end()
             |.append(${'$'}('<input type="hidden" name="$submitFuncName" value="_" />'))
             |.after(
             |  ${'$'}('<iframe id="$iframeId" name="$iframeId" />')
             |  .css('display','none')
             |  .load(function() {
             |    ${'$'}.globalEval(${'$'}(this).contents().text());
             |  })
             |);
            """.stripMargin
        )))
      }
    })
  }

  protected val editorFormRenderer = SHtml.idMemoize(renderer => editorTransforms())

  def renderEditor =
    ".editor-all" #> editorAllTemplate andThen
      ".editor-form [id]" #> id('form) andThen
      SHtml.makeFormsAjax andThen
      ".editor-form" #> editorFormRenderer
  //  andThen {
  //    viewableElems.collectFirst({ case e: FocusableElem => (ns: NodeSeq) => ns ++ <tail>{Script(Run( s"""if($$(':focus').size() == 0) {$$('#${e.focusId}').focus(); }"""))}</tail>}).getOrElse(PassThru)
  //  }

  @Deprecated
  def renderedNoBtns = (".editor-btn-submit" #> ClearNodes).apply(renderEditor(<div class="editor-all"></div>))

  def rendered = renderEditor(<div class="editor-all"></div>)

  def renderedEditor = rendered

  protected def savedInternal(): JsCmd = {
    modified = false
    saveFailed = false
    saved()
  }

  protected def saved(): JsCmd = Noop

  override def elemChanged(elem: HTMLViewableElem): JsCmd = super.elemChanged(elem) & {
    modified = true
    (viewableElems.map(_.update()) :+ Noop).reduce(_ & _)
  }

}

trait ImmediateChangesHTMLEditor extends HTMLEditorImpl {

  override def elemChanged(elem: HTMLViewableElem): JsCmd = super.elemChanged(elem) & {
    elem match {
      case elem: EditableElem if elem.error().isEmpty => elem.save()
      case _ =>
    }
  }
}

trait NoSubmitHTMLEditor extends HTMLEditorImpl {

  override protected def editorTransforms() = super.editorTransforms() andThen ".editor-btn-submit" #> ClearNodes
}

trait GlobalValidatableHTMLEditor extends HTMLEditorImpl {

  protected def globalError(): Option[NodeSeq] = None

  override protected def isValid: Boolean = fieldError().isEmpty && globalError().isEmpty

  protected var globalValidatableHTMLEditorShowGlobalError = false

  override protected def onFailedSaveAttempt(): Unit = {
    if (fieldError().isEmpty && !globalError().isEmpty)
      globalValidatableHTMLEditorShowGlobalError = true
    super.onFailedSaveAttempt()
  }

  override protected def onSucessfulSave(): Unit = {
    globalValidatableHTMLEditorShowGlobalError = false
    super.onSucessfulSave()
  }

  override protected def update() =
    super.update() & {
      globalError() match {
        case Some(error) if globalValidatableHTMLEditorShowGlobalError =>
          SetHtml(id('globalVal), error) & JsShowId(id('globalVal))
        case None => SetHtml(id('globalVal), NodeSeq.Empty) & JsHideId(id('globalVal))
        case _ => Noop
      }
    }

  override protected def editorTransforms() = super.editorTransforms() andThen ".editor-global-validation [id]" #> id('globalVal)
}

trait SemanticSubmitButtonHTMLEditor extends HTMLEditorImpl {

  def semanticInvalid = framework.btnDanger
  def semanticModified = framework.btnSuccess
  def semanticSaved = framework.btnMute

  def submitBtnSemanticClass =
    if (saveFailed && !isValid) semanticInvalid
    else if (modified) semanticModified
    else semanticSaved

  def submitBtnSemanticUpdate(): JsCmd = Run {
    Seq(semanticInvalid, semanticModified, semanticSaved)
      .map(clas => "$('#" + submitBtnRenderer.latestId + "').removeClass('" + clas + "');").mkString +
      "$('#" + submitBtnRenderer.latestId + "').addClass('" + submitBtnSemanticClass + "');"
  }

  override protected def update(): JsCmd = super.update() & submitBtnSemanticUpdate()

  override def submitBtnTransforms: NodeSeq => NodeSeq =
    super.submitBtnTransforms andThen
      ".editor-btn-submit [class+]" #> submitBtnSemanticClass

  override def elemChanged(elem: HTMLViewableElem): JsCmd = super.elemChanged(elem) & submitBtnSemanticUpdate()
}

trait DefaultHTMLEditor extends GlobalValidatableHTMLEditor with SemanticSubmitButtonHTMLEditor {
  type E = HTMLViewableElem
}

trait DefaultBS2HTMLEditor extends DefaultHTMLEditor with Bootstrap2 with Loc {
  def framework = new Bootstrap2 {}

  protected def onSave(): JsCmd

  override protected def saved() = super.saved() & onSave()
}

trait DefaultBS3HTMLEditor extends DefaultHTMLEditor with Bootstrap3 with LocC {
  def framework = new Bootstrap3 {}

  def n = "editor"

  protected def onSave(): JsCmd

  override protected def saved() = super.saved() & onSave()
}

trait EditableElem2DefaultEditorBridge extends HTMLEditableElem {

  protected def framework: Framework = editor.framework

  protected def editor: BasicHTMLEditor

  override def locPrefix = editor.locPrefix

  override def submit(): JsCmd = super.submit() & editor.submitForm()

  override def onChangeClientSide(): JsCmd = super.onChangeClientSide() & editor.elemChanged(this)

}
