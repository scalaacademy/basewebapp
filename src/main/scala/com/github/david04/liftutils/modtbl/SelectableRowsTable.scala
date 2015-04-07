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

import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.JsonAST.{JArray, JString, JValue}
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

import scala.xml.NodeSeq


trait SelectableRowsTable extends ClickableRowTable {

  override protected def tableClasses: List[String] = "selectable-rows-table" :: super.tableClasses

  protected def selectedRowClass = "selected"
  def initialSelectedRows = Set[R]()
  var selectedRows = initialSelectedRows

  protected def selectedRow(row: R): JsCmd = Noop
  protected def diselectedRow(row: R): JsCmd = Noop
  protected def changedSelection(selected: Set[R]): JsCmd = Noop

  override protected def trStylesFor(row: R, rowId: String, rowIdx: Int): List[String] =
    if (selectedRows.contains(row)) selectedRowClass :: super.trStylesFor(row, rowId, rowIdx)
    else super.trStylesFor(row, rowId, rowIdx)

  override protected def onClickClientSide(row: R, rowId: String, rowIdx: Int, col: C): JsCmd = Run(s"${'$'}('#$rowId').toggleClass('$selectedRowClass');")

  def onSelectClientSide(row: R, rowId: String, rowIdx: Int): JsCmd = Run(s"${'$'}('#$rowId').addClass('$selectedRowClass')")

  def onDiselectClientSide(row: R, rowId: String, rowIdx: Int): JsCmd = Run(s"${'$'}('#$rowId').removeClass('$selectedRowClass')")

  override protected def onClick(row: R, rowId: String, rowIdx: Int): JsCmd = {
    if (selectedRows.contains(row)) {
      selectedRows = selectedRows - row
      val r = diselectedRow(row) & changedSelection(selectedRows) & onDiselectClientSide(row, rowId, rowIdx)
      r
    } else {
      selectedRows = selectedRows + row
      val r = selectedRow(row)
      r & changedSelection(selectedRows) & onSelectClientSide(row, rowId, rowIdx)
    }
  }
}

trait MouseSelectableRowsTable extends SelectableRowsTable with RowIdsTable {
  type C <: MouseSelectableRowsCol

  protected val cols = collection.mutable.ListBuffer[(R, String, Int)]()

  trait MouseSelectableRowsCol extends ClickableRowCol {
    self: C =>
    override def clickableRowTransforms(row: R, rowId: String, rowIdx: Int, colId: String): NodeSeq => NodeSeq =
      if (isClickable(row, rowId, rowIdx, this)) "td [clickable]" #> "true"
      else PassThru
  }

  protected def selectCallback(rows: Map[String, R]) = SHtml.jsonCall(JsRaw("window.selected"), (v: JValue) => v match {
    case JArray(indexes) =>
      val selected = indexes.collect({ case JString(s) => rows(s)})
      selectedRows = selectedRows ++ selected
      selected.map(selectedRow(_)).foldLeft(Noop)(_ & _) & changedSelection(selectedRows)
    case _ => ???
  })

  protected def diselectCallback(rows: Map[String, R]) = SHtml.jsonCall(JsRaw("window.selected"), (v: JValue) => v match {
    case JArray(indexes) =>
      val selected = indexes.collect({ case JString(s) => rows(s)})
      selectedRows = selectedRows -- selected
      selected.map(diselectedRow(_)).foldLeft(Noop)(_ & _) & changedSelection(selectedRows)
    case _ => ???
  })

  override protected def rowTransforms(row: R, rId: String, rowIdx: Int, rows: Seq[R]): NodeSeq => NodeSeq =
    super.rowTransforms(row, rId, rowIdx, rows) andThen {
      if (isClickable(row, rId, rowIdx, columns.head)) {
        val rowIds = rows.map(r => rowId(r) -> r).toMap
        val id = rowId(row).encJs
        (ns: NodeSeq) => (ns ++ <tail>{Script(Run({
          "" +
            "if(!window.__mouseBtnPressedDetectorSet) {" +
            "  window.__mouseBtnPressedDetectorSet = true;" +
            "  $(document).mousedown(function(e){ if(e.which === 1) window.__mouseBtnPressed = true;  });" +
            "  $(document).mouseup  (function(e){ if(e.which === 1) window.__mouseBtnPressed = false; });" +
            "}" +
            "" +
            "$('#" + rId + " [clickable=true]').on('mousedown', function(e) {" +
            "  var before = $(this).parent('tr').hasClass('" + selectedRowClass + "');" +
            "  window.selecting = !e.shiftKey;" +
            "  if(e.shiftKey) {" +
            "    " + onDiselectClientSide(row, rId, rowIdx).toJsCmd +
            "  } else {" +
            "    " + onSelectClientSide(row, rId, rowIdx).toJsCmd +
            "  };" +
            s" window.selected = [$id];" +
            "  $(document).one('mouseup', function(e) {" +
            "    if(window.selected.length == 1 && window.selecting) {" +
            "      if(before) {" +
            "        " + onDiselectClientSide(row, rId, rowIdx).toJsCmd + ";" +
            "        " + diselectCallback(rowIds).toJsCmd +
            "      } else {" +
            "        " + onSelectClientSide(row, rId, rowIdx).toJsCmd + ";" +
            "        " + selectCallback(rowIds).toJsCmd +
            "      }" +
            "    } else {" +
            "      if(window.selecting) {" +
            "        " + selectCallback(rowIds).toJsCmd +
            "      } else {" +
            "        " + diselectCallback(rowIds).toJsCmd +
            "      }" +
            "    }" +
            "  });" +
            "});" +
            "$('#" + rId + "').on('mouseenter', function(e) {" +
            s" if(window.__mouseBtnPressed && window.selected && window.selected.indexOf($id) == -1) {" +
            s"   if(window.selecting) {" +
            s"     " + onSelectClientSide(row, rId, rowIdx).toJsCmd +
            s"   } else {" +
            s"     " + onDiselectClientSide(row, rId, rowIdx).toJsCmd +
            s"   };" +
            s"   window.selected.push($id);" +
            "  }" +
            "});"
        }))}</tail>)
      } else {
        PassThru
      }
    }
}
