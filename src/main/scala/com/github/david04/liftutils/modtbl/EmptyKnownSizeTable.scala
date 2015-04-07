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

import com.github.david04.liftutils.util.LUtils._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

import scala.xml._

trait EmptyTable extends Table {

  protected def emptyTableClass = "table-empty"

  protected def emptyTableContent: NodeSeq = Text(loc("empty"))

  override protected def rowsTransforms(rows: Seq[R]): Seq[NodeSeq => NodeSeq] = {
    if (rows.isEmpty) {
      ("td [class+]" #> emptyTableClass &
        "td [colspan]" #> columns.size &
        "td *" #> emptyTableContent) :: Nil
    } else {
      super.rowsTransforms(rows)
    }
  }
}

trait EmptyKnownSizeTable extends EmptyTable with KnownSizeTable {
//  def tableIsEmpty(rowsSizeOpt: Option[Long]): Boolean = rowsSizeOpt.map(_ == 0).getOrElse(false)
}

trait LoadingIndicatorTable extends Table {

  def loadingIndicator: String = "/static/images/ajax-loader/ajax-loader(11).gif"

  def toLoadingStatus(): JsCmd =
    SetHtml(id('table_body), <tr><td colspan={columns.size + ""} style="text-align:center;padding:20px;"><img src={loadingIndicator}></img></td></tr>) &
      SetHtml(id('modtbl_pag_info), scala.xml.Text("..."))

  override def rerenderPage(rowsSizeOpt: Option[Long] = rowsSizeOpt): JsCmd = toLoadingStatus() & SHtml.ajaxInvoke(() => %?("rerenderPage")(super.rerenderPage(rowsSizeOpt)))
  override def rerenderTable(): JsCmd = toLoadingStatus() & SHtml.ajaxInvoke(() => %?("rerenderTable")(super.rerenderTable()))
  override def rerenderTableBody(): JsCmd = toLoadingStatus() & SHtml.ajaxInvoke(() => %?("rerenderTableBody")(super.rerenderTableBody()))
}