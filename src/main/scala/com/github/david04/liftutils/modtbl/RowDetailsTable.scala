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

package com.github.david04.liftutils.modtbl

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

import scala.xml.NodeSeq


trait RowDetailsTable extends ClickableRowTable {

  var currentDetailsRow: Option[(R, JsCmd)] = None

  protected def rowDetailsClasses: List[String] = "details" :: Nil

  protected def rowDetailsContentClass: String = "details-contents"

  protected def rowDetailsContents(row: R, rowId: String, rowIdx: Int): NodeSeq

  def openDetailsJs(sel: String) = sel + ".slideDown(400);"

  def closeDetailsJs(sel: String, andThen: String) = sel + ".slideUp(400, function() {" + andThen + "});"

  protected def openDetailsRow(row: R, rowId: String, rowIdx: Int): JsCmd = Run {
    val ns = <tr><td><div class={rowDetailsContentClass}>{rowDetailsContents(row, rowId, rowIdx)}</div></td></tr>
    sel(rowId, ".after(" + rowDetailsTransforms(row, rowId, rowIdx, false)(ns).toString().encJs + ");") +
      openDetailsJs(sel(s"$rowId-details .$rowDetailsContentClass"))
  }

  protected def closeDetailsRow(row: R, rowId: String, rowIdx: Int): JsCmd = Run {
    closeDetailsJs(sel(s"$rowId-details .$rowDetailsContentClass"), sel(s"$rowId-details", ".remove();"))
  }

  override protected def onClick(row: R, rowId: String, rowIdx: Int): JsCmd = {
    currentDetailsRow match {
      case Some((prev, close)) if prev == row =>
        currentDetailsRow = None
        close
      case None =>
        currentDetailsRow = Some((row, closeDetailsRow(row, rowId, rowIdx)))
        openDetailsRow(row, rowId, rowIdx)
      case Some((_, close)) =>
        currentDetailsRow = Some((row, closeDetailsRow(row, rowId, rowIdx)))
        close & openDetailsRow(row, rowId, rowIdx)
    }
  }

  protected def rowDetailsTransforms(row: R): NodeSeq => NodeSeq = PassThru

  protected def rowDetailsTransforms(row: R, rowId: String, rowIdx: Int, visible: Boolean): NodeSeq => NodeSeq =
    "tr [id]" #> s"$rowId-details" &
      s"tr .$rowDetailsContentClass [style+]" #> (if (visible) "" else ";display:none;") andThen
      "td [colspan]" #> columns.size &
        "td [class+]" #> rowDetailsClasses.mkString(" ") &
        "td" #> rowDetailsTransforms(row)

  override protected def rowTransforms(row: R, rowId: String, rowIdx: Int, rows: Seq[R]): NodeSeq => NodeSeq =
    super.rowTransforms(row, rowId, rowIdx, rows) andThen {
      currentDetailsRow match {
        case Some((open, _)) if open == row =>
          val detailsNs = <tr><td><div class={rowDetailsContentClass}>{rowDetailsContents(row, rowId, rowIdx)}</div></td></tr>
          currentDetailsRow = Some((row, closeDetailsRow(row, rowId, rowIdx)))
          (ns: NodeSeq) => ns ++ rowDetailsTransforms(row, rowId, rowIdx, true)(detailsNs)
        case _ =>
          PassThru
      }
    }
}
