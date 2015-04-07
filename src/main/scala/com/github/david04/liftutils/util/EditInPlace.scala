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

package com.github.david04.liftutils.util

import java.util.regex.Pattern

import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.JsonAST.JString
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

class InPlace1[T](
                   get: => T,
                   toStr: T => String,
                   set: T => JsCmd,
                   fromStr: String => Option[T],
                   ifEmpty: String = "",
                   setParentWidth: Boolean = false,
                   serverVal: Boolean = true,
                   startSelected: Boolean = false
                   ) {

  def render = {
    def current = Some(toStr(get)).filter(_ != "").getOrElse(ifEmpty)

    val id = Helpers.nextFuncName

    val selectText =
      s"""if(!window.$id) {
                          |window.$id = true;
                                       |var range, selection;
                                       |if (document.body.createTextRange) {
                                       |range = document.body.createTextRange();
                                       |range.moveToElementText(this);
                                       |range.select();
                                       |} else if (window.getSelection) {
                                       |selection = window.getSelection();
                                       |range = document.createRange();
                                       |range.selectNodeContents(this);
                                       |selection.removeAllRanges();
                                       |selection.addRange(range);
                                       |};
                                       |}
                                       | """.stripMargin

    val selectText2 =
      s"""
         |var range, selection;
         |if (document.body.createTextRange) {
         |range = document.body.createTextRange();
         |range.moveToElementText(document.getElementById("$id"));
                                                                |range.select();
                                                                |} else if (window.getSelection) {
                                                                |selection = window.getSelection();
                                                                |range = document.createRange();
                                                                |range.selectNodeContents(document.getElementById("$id"));
                                                                                                                        |selection.removeAllRanges();
                                                                                                                        |selection.addRange(range);
                                                                                                                        |};
                                                                                                                        |""".stripMargin

    lazy val save: String = {
      Run(s"window.$id = false;" +
        s"$$('#$id')" +
        s".attr('contenteditable','false')" +
        s".attr('class','inplace-display')" +
        s".removeAttr('onblur');") &
        SHtml.ajaxCall(JsRaw(s"$$('#$id').text()"), v => {
          Some(v).filter(_ != ifEmpty || ifEmpty == "").flatMap(fromStr(_)).map(set(_)).getOrElse(Noop) &
            Run(s"$$('#$id')" +
              s".attr('onfocus'," + edit.encJs + ")" +
              s".attr('onclick'," + (/*selectText +*/ edit).encJs + ")" +
              s".text(${current.encJs});")
        }).cmd
    }.toJsCmd

    lazy val edit = {
      Run(s"$$('#$id')" +
        s".attr('class','inplace-edit')" +
        s".attr('onblur'," + save.encJs + ")" +
        s".attr('contenteditable','true')" +
        ";")
    }.toJsCmd

    <div style="display:inline;overflow: hidden;" tabindex="0" id={id} class="inplace-display" onfocus={/*selectText + */ edit} onclick={edit}>{current}</div> ++
      <tail>{Script(OnLoad(Run(s"$$('#$id').keydown(function(event){if(event.keyCode == 13){" + save + "; return false;}});") & (if (startSelected) Run(edit /*+ selectText2*/) else Noop)))}</tail>
  }

}

class InPlace[T](
                  get: => T,
                  toStr: T => String,
                  set: T => JsCmd,
                  fromStr: String => Option[T],
                  ifEmpty: String = "",
                  setParentWidth: Boolean = false,
                  serverVal: Boolean = true,
                  startSelected: Boolean = false,
                  cls: String = ""
                  ) {

  val id = Helpers.nextFuncName

  object clas {
    def +=(cls: String): JsCmd = Run( s"""$$('#$id').addClass(${cls.encJs});""")
    def -=(cls: String): JsCmd = Run( s"""$$('#$id').removeClass(${cls.encJs});""")
  }

  def render = {
    var editing = false

    val selectText2 = Run {
      s"""setTimeout(function() {
         |var el = document.getElementById("$id");
                                                 |var range = document.createRange();
                                                 |range.selectNodeContents(el);
                                                 |var sel = window.getSelection();
                                                 |sel.removeAllRanges();
                                                 |sel.addRange(range);
                                                 |el.focus();
                                                 |}, 30);
                                                 |""".stripMargin
    }

    lazy val save: String = {
      SHtml.ajaxCall(JsRaw(s"$$('#$id').text()"), v => {
        editing = false
        Some(v).filter(_ != ifEmpty || ifEmpty == "").flatMap(fromStr(_)).map(set(_)).getOrElse(Noop) &
          renderer.setHtml()
      }).cmd
    }.toJsCmd

    lazy val renderer = SHtml.idMemoize(renderer => (_: NodeSeq) => {
      def current = Some(toStr(get)).filter(_ != "").getOrElse(ifEmpty)

      lazy val editMode = <div style="display:inline;" tabindex="0" id={id} class={"inplace-edit " + cls} contenteditable="true" onblur={save} onkeydown={"function(event){if(event.keyCode == 13){" + save + "; return false;}}"}>{current}</div>
      lazy val viewMode = <div style="display:inline;" tabindex="0" id={id} class={"inplace-display " + cls} onclick={SHtml.ajaxInvoke(() => {editing = true; renderer.setHtml() & selectText2}).toJsCmd}>{current}</div>

      if (editing) editMode else viewMode
    })

    renderer.apply(<div style="display:inline;"></div>)
  }

}


class InPlaceOpt[T](
                     get: => Option[T],
                     toStr: T => String,
                     set: Option[T] => JsCmd,
                     fromStr: String => Option[T],
                     ifEmpty: String = "",
                     setParentWidth: Boolean = false,
                     serverVal: Boolean = true
                     ) extends
InPlace[Option[T]](
  get,
  _.map(toStr).getOrElse(""),
  set,
  s => if (s == "") Some(None) else fromStr(s).map(Some(_)),
  ifEmpty,
  setParentWidth,
  serverVal)

class InPlaceSelect[T](
                        all: Seq[T],
                        toStr: T => String,
                        selected: => T,
                        set: T => JsCmd,
                        displayRenderer: T => String
                        ) {

  def render = {
    val displayId = Helpers.nextFuncName
    val editId = Helpers.nextFuncName

    val display = Run(s"$$('#$editId').hide();$$('#$displayId').show();")
    val edit = Run(s"$$('#$displayId').hide();$$('#$editId').show();").toJsCmd

    val onselect =
      SHtml.ajaxCall(
        JsRaw(s"$$('#$editId').val()"),
        (v: String) => {
          tryo(v.toInt).map(v => {
            set(all(v)) &
              SetValById(editId, JString(all.indexOf(selected).toString)) &
              Run(s"$$('#$displayId').text(${displayRenderer(selected).encJs});") &
              display
          }).getOrElse(Noop)
        }).toJsCmd

    <span class="inplace-display" onclick={edit} id={displayId}>{displayRenderer(selected)}</span> ++
      <select class="inplace-edit" id={editId} onblur={display.toJsCmd} style="display:none">{
        all.zipWithIndex.map({
          case (v, idx) if selected == v => <option selected="selected" value={idx.toString}>{toStr(v)}</option>
          case (v, idx) => <option value={idx.toString}>{toStr(v)}</option>
        })
      }</select> ++
     <tail>{Script(OnLoad(Run(s"$$('#$editId').change(function(event){" + onselect + "});")))}</tail>
  }
}

class InPlaceGroupSelect[T](
                             all: Seq[(String, Seq[T])],
                             toStr: T => String,
                             selected: => T,
                             set: T => JsCmd,
                             displayRenderer: T => String
                             ) {

  def render = {
    val displayId = Helpers.nextFuncName
    val editId = Helpers.nextFuncName

    val display = Run(s"$$('#$editId').hide();$$('#$displayId').show();")
    val edit = Run(s"$$('#$displayId').hide();$$('#$editId').show();").toJsCmd

    val onselect =
      SHtml.ajaxCall(
        JsRaw(s"$$('#$editId').val()"),
        (v: String) => {
          tryo(v.split(",").map(_.toInt)).map(v => {
            set(all(v(0))._2(v(1))) &
              SetValById(editId, JString(all.indexOf(selected).toString)) &
              Run(s"$$('#$displayId').text(${displayRenderer(selected).encJs});") &
              display
          }).getOrElse(Noop)
        }).toJsCmd

    <span class="inplace-display" onclick={edit} id={displayId}>{displayRenderer(selected)}</span> ++
      <select class="inplace-edit" id={editId} onblur={display.toJsCmd} style="display:none">{
        all.zipWithIndex.map(group => {
          val ns = group._1._2.zipWithIndex.map({
            case (v, idx) if selected == v => <option selected="selected" value={group._2.toString + "," + idx.toString}>{toStr(v)}</option>
            case (v, idx) => <option value={group._2.toString + "," + idx.toString}>{toStr(v)}</option>
          })
          <optgroup label={group._1._1}>{ns}</optgroup>
        })
      }</select> ++
     <tail>{Script(OnLoad(Run(s"$$('#$editId').change(function(event){" + onselect + "});")))}</tail>
  }
}

object InPlace {

  def str(get: => String, set: String => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, serverVal: Boolean = true, startSelected: Boolean = false) =
    if (S.inStatefulScope_?)
      new InPlace[String](get, s => s, set, s => Some(s), ifEmpty, setParentWidth, serverVal, startSelected).render
    else <span>{get}</span>

  def strOpt(get: => Option[String], set: Option[String] => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, serverVal: Boolean = true) =
    if (S.inStatefulScope_?)
      new InPlaceOpt[String](get, s => s, set, s => Some(s), ifEmpty, setParentWidth, serverVal).render
    else <span>{get}</span>

  def pw(set: String => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, serverVal: Boolean = true) =
    if (S.inStatefulScope_?)
      new InPlace[String]("", s => s, set, s => Some(s), ifEmpty, setParentWidth, serverVal).render
    else <span>······</span>

  def double(get: => Double, set: Double => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, serverVal: Boolean = true, append: String = "", fmt: String = "%.2f") =
    if (S.inStatefulScope_?)
      new InPlace[Double](get, _.formatted(fmt) + append, set, s => tryo(s.replaceAll(Pattern.quote(append) + "$", "").toDouble).toOption, ifEmpty, setParentWidth, serverVal).render
    else <span>{get.formatted(fmt) + append}</span>

  def select[T](all: Seq[T], toStr: T => String, selected: => T, set: T => JsCmd, displayRenderer: T => String) =
    if (S.inStatefulScope_?)
      new InPlaceSelect[T](all, toStr, selected, set, displayRenderer).render
    else <span>{toStr(selected)}</span>

  def select[T](all: Seq[T], toStr: T => String, selected: => T, set: T => JsCmd) =
    if (S.inStatefulScope_?)
      new InPlaceSelect[T](all, toStr, selected, set, toStr).render
    else <span>{toStr(selected)}</span>

  def groupSelect[T](all: Seq[(String, Seq[T])], toStr: T => String, selected: => T, set: T => JsCmd, displayRenderer: T => String) =
    if (S.inStatefulScope_?)
      new InPlaceGroupSelect[T](all, toStr, selected, set, displayRenderer).render
    else <span>{toStr(selected)}</span>

  def enum[E <: Enumeration](enum: E, selected: => E#Value, set: E#Value => JsCmd) =
    if (S.inStatefulScope_?)
      new InPlaceSelect[E#Value](enum.values.toSeq, _.toString, selected, set, _.toString).render
    else <span>{selected.toString}</span>

  def enumOpt[E <: Enumeration](enum: E, selected: => Option[E#Value], set: Option[E#Value] => JsCmd, none: String) =
    if (S.inStatefulScope_?) {
      new InPlaceSelect[Option[E#Value]](enum.values.toSeq.map(Some(_)) :+ None, _.map(_.toString).getOrElse(none), selected, set, _.map(_.toString).getOrElse(none)).render
    } else {
      <span>{selected.map(_.toString).getOrElse(none)}</span>
    }

}
