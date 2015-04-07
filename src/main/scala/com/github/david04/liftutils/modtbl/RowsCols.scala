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

import com.github.david04.liftutils.loc.Loc
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

trait NamedColTable extends Table {
  type C <: NamedCol

  trait NamedCol extends TableCol {
    self: C =>
    def name: String
    def locPrefix = name

    override def renderHead: NodeSeq => NodeSeq = super.renderHead andThen "th [col-name]" #> name

    override def renderRow(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq => NodeSeq =
      super.renderRow(row, rowId, rowIdx, colId, colIdx, rrdRow) andThen "td [col-name]" #> name
  }

}

trait RowIdTable extends Table {
  type R <: {def id: String}
}

trait NodeSeqHeadTable extends Table {
  type C <: NodeSeqHeadCol

  trait NodeSeqHeadCol extends TableCol {
    self: C =>
    def headNodeSeqValue(): NodeSeq

    def nodeSeqHeadTableTransforms() =
      "th *" #> headNodeSeqValue()

    override def renderHead: NodeSeq => NodeSeq = super.renderHead andThen nodeSeqHeadTableTransforms()
  }

}

trait CachedNodeSeqHeadTable extends NodeSeqHeadTable {
  type C <: CachedNodeSeqHeadCol

  trait CachedNodeSeqHeadCol extends NodeSeqHeadCol {
    self: C =>

    protected lazy val nodeSeqHeadCache = headNodeSeqValue()

    override def nodeSeqHeadTableTransforms() = "th *" #> nodeSeqHeadCache
  }

}

trait StrHeadTable extends NodeSeqHeadTable {
  type C <: StrHeadCol

  trait StrHeadCol extends NodeSeqHeadCol {
    self: C =>
    def title: String

    def headNodeSeqValue: NodeSeq = scala.xml.Text(title)
  }

}

trait LocStrHeadTable extends StrHeadTable with NamedColTable with Loc {
  type C <: LocStrHeadCol

  trait LocStrHeadCol extends StrHeadCol with NamedCol {
    self: C =>

    def title = loc("title")
  }

}

trait NodeSeqRowTable extends Table {

  trait NodeSeqRowCol extends TableCol {
    self: C =>
    def rowNodeSeqValue(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq

    def nodeSeqRowTableTransforms(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd) = "td *" #> rowNodeSeqValue(row, rowId, rowIdx, colId, colIdx, rrdRow)

    override def renderRow(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq => NodeSeq =
      super.renderRow(row, rowId, rowIdx, colId, colIdx, rrdRow) andThen
        nodeSeqRowTableTransforms(row, rowId, rowIdx, colId, colIdx, rrdRow)
  }

}

trait CachedNodeSeqRowTable extends NodeSeqRowTable {
  type C <: CachedNodeSeqRowCol

  trait CachedNodeSeqRowCol extends NodeSeqRowCol {
    self: C =>

    protected val nodeSeqRowCache = collection.mutable.Map[R, NodeSeq]()

    override def nodeSeqRowTableTransforms(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd) =
      "td *" #> nodeSeqRowCache.getOrElseUpdate(row, rowNodeSeqValue(row, rowId, rowIdx, colId, colIdx, rrdRow))
  }

}

trait StrRowTable extends NodeSeqRowTable {

  trait StrRowCol extends NodeSeqRowCol {
    self: C =>
    def rowStrValue: R => String

    def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = scala.xml.Text(rowStrValue(r))
  }

}
