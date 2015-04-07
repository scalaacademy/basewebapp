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

import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait AdditionalDetailsTable extends Table {

  protected def nonDetailsRowClass = "normal-tr"
  override protected def trStyle = nonDetailsRowClass :: super.trStyle

  protected def rowDetailsContents(row: R, rowId: String, rowIdx: Int): NodeSeq = NodeSeq.Empty

  protected def rowDetails(row: R, rowId: String, rowIdx: Int): NodeSeq => NodeSeq =
    "tr [id]" #> (rowId + "_details") &
      "td [colspan]" #> columns.size &
      "td *" #> rowDetailsContents(row, rowId, rowIdx)

  override protected def rowsTransforms(rows: Seq[R]): Seq[NodeSeq => NodeSeq] =
    rows.zipWithIndex.flatMap(row => {
      val id = Helpers.nextFuncName
      rowTransforms(row._1, id, row._2, rows) ::
        rowDetails(row._1, id, row._2) :: Nil
    })
}
