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
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

import scala.xml.NodeSeq

trait ClickableRowTable extends Table {
  type C <: ClickableRowCol

  protected def onClickClientSide(row: R, rowId: String, rowIdx: Int, col: C): JsCmd = JsCmds.Noop

  protected def onClick(row: R, rowId: String, rowIdx: Int): JsCmd = JsCmds.Noop

  trait NonClickableCol {
    self: C =>
  }

  def isClickable(row: R, rowId: String, rowIdx: Int, col: C): Boolean = !col.isInstanceOf[NonClickableCol]

  def clickableTdClasses: List[String] = "clickable" :: Nil

  trait ClickableRowCol extends TableCol {
    self: C =>

    def clickableRowTransforms(row: R, rowId: String, rowIdx: Int, colId: String): NodeSeq => NodeSeq = {
      if (isClickable(row, rowId, rowIdx, this))
        "td [class+]" #> clickableTdClasses.mkString(" ") &
          "td [onclick]" #>
            (onClickClientSide(row, rowId, rowIdx, this) &
              SHtml.onEvent(_ => onClick(row, rowId, rowIdx)).cmd).toJsCmd
      else PassThru
    }

    override def renderRow(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq => NodeSeq =
      super.renderRow(row, rowId, rowIdx, colId, colIdx, rrdRow) andThen
        clickableRowTransforms(row, rowId, rowIdx, colId)
  }

}

trait ColClickableRowTable extends Table {
  type C <: ClickableRowCol

  protected def onClickClientSide(row: R, rowId: String, rowIdx: Int, col: C, colId: String): JsCmd = JsCmds.Noop

  protected def onClick(row: R, rowId: String, rowIdx: Int, col: C, colId: String): JsCmd = JsCmds.Noop

  def isClickable(row: R, rowId: String, rowIdx: Int): Boolean = true

  trait ClickableRowCol extends TableCol {
    self: C =>

    def clickableRowTransforms(row: R, rowId: String, rowIdx: Int, colId: String): NodeSeq => NodeSeq = {
      if (isClickable(row, rowId, rowIdx))
        "td [onclick]" #>
          (onClickClientSide(row, rowId, rowIdx, this, colId) &
            SHtml.onEvent(_ => onClick(row, rowId, rowIdx, this, colId)).cmd).toJsCmd
      else PassThru
    }

    override def renderRow(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq => NodeSeq =
      super.renderRow(row, rowId, rowIdx, colId, colIdx, rrdRow) andThen
        clickableRowTransforms(row, rowId, rowIdx, colId)
  }

}