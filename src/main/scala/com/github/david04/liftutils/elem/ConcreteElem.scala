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


import com.github.david04.liftutils.reactive.{JxStr, SJxVar}
import net.liftweb.common._
import net.liftweb.http.S.{LFuncHolder, SFuncHolder}
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.http.js.JE.{JsRaw, ValById}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.{FileParamHolder, S, SHtml}
import net.liftweb.json.JsonAST.{JArray, JNull, JString, JValue}
import net.liftweb.util.Helpers._
import org.apache.commons.lang3.StringEscapeUtils

import scala.xml.{NodeSeq, UnprefixedAttribute}

trait PasswordInputElem extends TextInputElem {

  override protected def inputElem: NodeSeq = ("input [type]" #> "password").apply(super.inputElem)
}

trait TextInputElem extends GenEditableStringValueElem with HTMLEditableElem with LabeledElem with SJxVar[String] with JxStr with FocusableElem {

  protected def placeholder: Option[String]

  protected val initialRx = getStringValue()
  protected val toJx = (s: String) => JE.Str(s)
  protected val fromJx = (v: JValue) => v.asInstanceOf[JString].s

  def getCurrentStringValue(): String = getRx

  protected def textInputAttrs: Seq[ElemAttr]

  protected def classes: List[String] = Nil

  import ElemAttr._

  override def focusId: String = id('input)

  protected def detectPressReturn: Boolean = true

  protected def inputElemDefaultAttrs: Seq[ElemAttr] = Seq[ElemAttr](
    ("id" -> id('input)),
    ("placeholder" -> placeholder.getOrElse("")),
    ("onchange" -> ("{" + setJx(JsRaw("this.value+''"), Noop, () => onChangeClientSide()).toJsCmd + "; return true; }")),
    ("onkeyup" -> (setJx(JsRaw(ValById(id('input)).toJsCmd + "+''")).toJsCmd))
  ) ++
    (if (detectPressReturn) List(ElemAttr.pairToBasic("onkeydown" -> s"if (event.keyCode == 13) {${SHtml.ajaxInvoke(() => submit()).toJsCmd}}; return true;")) else Nil)

  protected def inputElem: NodeSeq = SHtml.text(getRx, s => setRx(s), textInputAttrs ++ inputElemDefaultAttrs: _*)

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen
      ((ns: NodeSeq) => bind("elem", ns, "input" -%> inputElem)) andThen
      ((ns: NodeSeq) => ns ++ initRXScript())
}

trait TextAreaInputElem extends TextInputElem {
  override protected def inputElem: NodeSeq = SHtml.textarea(getRx, s => setRx(s), textInputAttrs ++ inputElemDefaultAttrs: _*)
}

trait TextViewerElem extends GenStringValueElem with HTMLViewableElem with LabeledElem {

  protected def classes: List[String] = Nil


  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen
      ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error) &
        ".elem-value *" #> getStringValue()
}

trait SelectInputElem extends GenOneOfManyValueElem with HTMLEditableElem with LabeledElem with FocusableElem {

  override def reloadValue() = {
    val v = getOneOfManyValue()
    setCurrentOneOfManyValue(_.id == v.id)
    Run(s"$$('#${id('input)}').val(${v.id.encJs});")
  }

  override def focusId: String = id('input)

  def select(opts: Seq[(String, NodeSeq)], deflt: Box[String],
             _func: String => Any, attrs: ElemAttr*): scala.xml.Elem = {
    def selected(in: Boolean) =
      if (in) new UnprefixedAttribute("selected", "selected", scala.xml.Null) else scala.xml.Null

    val func = SFuncHolder(_func)
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)

    attrs.foldLeft(S.fmapFunc(testFunc)(fn => <select name={fn}>
      {opts.flatMap {
        case (value, text) => (<option value={value}>
          {StringEscapeUtils.unescapeHtml4(text.text)}
        </option>) % selected(deflt.exists(_ == value))
      }}
    </select>))(_ % _)
  }

  private var value: OneOfManyValue = getOneOfManyValue()

  def getCurrentOneOfManyValue() = value

  def setCurrentOneOfManyValue(p: OneOfManyValue => Boolean): JsCmd = {
    value = getAllOneOfManyValues().find(p).get
  }

  protected def selectInputAttrs: Seq[ElemAttr]

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen
      ((ns: NodeSeq) => bind("elem", ns, "input" -%>
        select(
          getAllOneOfManyValues().map(v => (v.id, v.name)), Full(value.id),
          v => getAllOneOfManyValues().find(_.id == v).foreach(value = _),
          (selectInputAttrs ++ Seq[ElemAttr](
            ("id" -> id('input)),
            ("onchange" -> ("{" + SHtml.onEvent(v => {getAllOneOfManyValues().find(_.id == v).foreach(value = _); onChangeClientSide()}).toJsCmd + "; return true; }"))
          )): _*)
      ))
}

trait RadioInputElem extends GenOneOfManyValueElem with HTMLEditableElem with LabeledElem {

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-radio-dflt" :: Nil

  private var value: OneOfManyValue = getOneOfManyValue()

  def getCurrentOneOfManyValue() = value

  def setCurrentOneOfManyValue(p: OneOfManyValue => Boolean): JsCmd = {
    value = getAllOneOfManyValues().find(p).get
  }

  def radioStartsUninitialized = false

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *+" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen {
      ".elem-repeat" #> getAllOneOfManyValues().map(v => (v.id, v.name)).map(e => (ns: NodeSeq) => {
        val (id, name) = e
        bind("elem", (".elem-label *+" #> name.toString()).apply(ns), "input" -%>
          <input type="radio"
            name={this.id('name)}
            checked={if (!radioStartsUninitialized && value.id == id) "true" else null}
            onclick={
              SHtml.ajaxInvoke(() => {getAllOneOfManyValues().find(_.id == id).foreach(value = _); onChangeClientSide()}).toJsCmd
            }
            value=""></input>
        )
      })
    }
}

trait MultiSelectInputElem extends GenManyOfManyValueElem with HTMLEditableElem with LabeledElem with FocusableElem {

  private var value = getManyOfManyValue()

  def getCurrentManyOfManyValue() = value

  protected def selectInputAttrs: Seq[ElemAttr]

  override def requiresIFrameSubmit: Boolean = true

  override def focusId: String = id('input)

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen
      ((ns: NodeSeq) => bind("elem", ns, "input" -%>
        SHtml.multiSelectObj[String](
          getAllManyOfManyValues().map(v => (v.id, v.name.toString())),
          value.map(_.id),
          (v: List[String]) => {
            val map = getAllManyOfManyValues().map(v => (v.id, v)).toMap
            value = v.flatMap(map.get(_))
          },
          (selectInputAttrs ++ Seq[ElemAttr](
            ("id" -> id('input)),
            ("onchange" -> ("{" + SHtml.jsonCall(JsRaw(sel('input, ".val()")),
              (v: JValue) => v match {
                case JArray(lst) =>
                  val map = getAllManyOfManyValues().map(v => (v.id, v)).toMap
                  value = lst.collect({ case JString(v) => v}).flatMap(map.get(_))
                  onChangeClientSide()
                case JNull =>
                  value = Seq()
                  onChangeClientSide()
                case other =>
                  println(other)
                  ???
              }).toJsCmd + "; return true; }"))
          )): _*)
      ))
}

trait FileUploadInputElem extends GenFileOptValueElem with HTMLEditableElem with LabeledElem with FocusableElem {

  private var value: Option[(Array[Byte], String)] = None

  def getFile() = value

  protected def fileInputAttrs: Seq[ElemAttr]

  override def requiresIFrameSubmit = true

  override def focusId: String = id('input)

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen
      ((ns: NodeSeq) => bind("elem", ns, "input" -%>
        SHtml.fileUpload(
          (file: FileParamHolder) => {
            println("RECEIVED FILE: " + file.fileName)
            value = Some((file.file, file.fileName))
          },
          (fileInputAttrs ++ Seq[ElemAttr](
            ("id" -> id('input))
          )): _*)
      ))
}

trait CheckboxInputElem extends GenEditableBooleanValueElem with HTMLEditableElem with LabeledElem with FocusableElem {

  override protected def wrapName(name: String) = name

  private var value = getBooleanValue()

  def getCurrentBooleanValue() = value

  protected def checkboxInputAttrs: Seq[ElemAttr]

  override def focusId: String = id('input)

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *+" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen
      ((ns: NodeSeq) => bind("elem", ns, "input" -%>
        SHtml.checkbox(value, value = _,
          (checkboxInputAttrs ++ Seq[ElemAttr](
            ("id" -> id('input)),
            ("onchange" -> ("{" + SHtml.ajaxCall(JsRaw(sel('input) + ".is(':checked')"),
              v => {value = v.toBoolean; onChangeClientSide()}).toJsCmd + "; return true; }"))
          )): _*)
      ))
}

trait MultiCheckboxInputElem extends GenManyOfManyValueElem with HTMLEditableElem with LabeledElem {

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-multi-checkbox-dflt" :: Nil

  private var value = getManyOfManyValue()

  def getCurrentManyOfManyValue() = value

  protected def checkboxInputAttrs: Seq[ElemAttr]

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *+" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error)
      ) andThen {
      ".elem-repeat" #> getAllManyOfManyValues().zipWithIndex.map(e => (ns: NodeSeq) => {
        val (v, idx) = e
        bind("elem", (".elem-label *+" #> v.name.toString()).apply(ns), "input" -%>
          SHtml.checkbox(
            value.contains(v),
            selected => if (selected) {if (!value.contains(v)) value = value :+ v} else value = value.filter(_ != v),
            (checkboxInputAttrs ++ Seq[ElemAttr](
              ("id" -> id(Symbol("input" + idx))),
              ("onchange" -> ("{" + SHtml.ajaxCall(JsRaw(sel(Symbol("input" + idx)) + ".is(':checked')"),
                selected => {
                  if (selected == "true") value = value :+ v else value = value.filter(_ != v)
                  onChangeClientSide()
                }).toJsCmd + "; return true; }"))
            )): _*))
      })
    }

}

trait IconElem extends HTMLEditableElem {def icon: String}

trait HTMLIIconElem extends IconElem {
  override def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen
      "i [class]" #> s"$icon"
}

