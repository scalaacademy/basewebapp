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
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

trait ReordableRowsTable extends Table {

  def isReordable(row: R): Boolean = true

  def reordered(row: R, idx: Int): JsCmd = Noop

  override def tableClasses: List[String] = "modtbl-reordable-rows" :: super.tableClasses

  override protected def trStylesFor(row: R, rowId: String, rowIdx: Int): List[String] =
    if (isReordable(row)) super.trStylesFor(row, rowId, rowIdx)
    else "reorder-disabled" :: super.trStylesFor(row, rowId, rowIdx)

  override protected def rowTransforms(row: R, rowId: String, rowIdx: Int, rows: Seq[R]): NodeSeq => NodeSeq =
    super.rowTransforms(row, rowId, rowIdx, rows) andThen
      "tr [reorder]" #> SHtml.ajaxCall(JsRaw("idx"), idx => {
        reordered(row, idx.toInt) & rerenderTable()
      }).toJsCmd

  def reorderRowMinDistancePx = 15

  override protected def tableTransforms(): NodeSeq => NodeSeq =
    super.tableTransforms() andThen
      ((ns: NodeSeq) => ns ++
        <tail>{
          Script(Run(
            "$('#" + id('table) + " tbody')" +
              ".sortable({" +
              s"items: 'tr:not(.reorder-disabled)'," +
              s"distance: $reorderRowMinDistancePx," +
              "update: " +
              "  function( event, ui ) {" +
              "    var sorted = " +
              "      $.map($.map($('#" + id('table) + " tr'), " +
              "        function(r) {return {id: $(r).attr('id'), offset: r.offsetTop};})" +
              "          .sort(function(a,b){return a.offset-b.offset;}), function(r) {return r.id;});" +
              "    var idx = sorted.indexOf(ui.item.attr('id'));" +
              "    window.sorted = sorted;" +
              "    window.idx = idx;" +
              "    eval('0, ' + ui.item.attr('reorder'));" +
              "  }," +
              "helper:" +
              "  function(e, ui) {" +
              "    ui.children().each(function() {" +
              "      $(this).width($(this).width());" +
              "    });" +
              "    return ui;" +
              "  }" +
              "})"))
        }</tail>)
}
