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

import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

trait DateTimePickerInput extends HTMLEditableElem with LabeledElem {
  implicit def self = this

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-datetimepicker-dflt" :: Nil

  def get: () => (Long, Long)

  def set: ((Long, Long)) => JsCmd

  var value = get()

  def save(): JsCmd = set(getCurrentValue())

  def getCurrentValue(): (Long, Long) = value

  def setupDatePicker(dpId: Symbol, initial: Long, set: Long => JsCmd) = {
    val v = "window." + Helpers.nextFuncName
    Script(OnLoad(Run("" +
      s"$v = ${sel(dpId)}.datetimepicker({autoclose: true, isRTL: false, format: 'dd/mm/yy - hh:ii', pickerPosition: 'bottom-right'})" +
      "  .on('changeDate', function(e) {" + SHtml.ajaxCall(JsRaw("e.date.getTime()"), v => tryo(v.toLong).map(set(_)).openOr(Noop)).toJsCmd + "});" +
      s"$v.data('datetimepicker').setDate(new Date($initial));"
    )))
  }

  override protected def htmlElemRendererTransforms =
    super.htmlElemRendererTransforms andThen (
      ".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".timerangepicker-from [onkeydown]" #> "return false;" &
        ".timerangepicker-to [onkeydown]" #> "return false;" &
        ".timerangepicker-from [id]" #> id('from) &
        ".timerangepicker-to [id]" #> id('to) &
        ".elem-error [id]" #> id('error)
      ) andThen {
      (ns: NodeSeq) => ns ++
        setupDatePicker('from, value._1, v => {value = (v, value._2); onChangeClientSide()}) ++
        setupDatePicker('to, value._2, v => {value = (value._1, v); onChangeClientSide()})
    }
}
