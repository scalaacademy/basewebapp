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
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.util.Helpers._
import net.liftweb.util.{FatLazy, PassThru}

import scala.xml.NodeSeq


trait SortableQueryableTable extends QueryableTable with NamedColTable {

  protected def sortThNone = "sorting"

  protected def sortThAsc = "sorting_asc"

  protected def sortThDesc = "sorting_desc"

  trait SortCol extends NamedCol {
    self: C =>

    def defaultSortAsc: Boolean = true

    def sortable: Boolean

    override def renderHead: NodeSeq => NodeSeq =
      super.renderHead andThen
        (if (sortable)
          "th [class+]" #> (if (name == currentSortCol.name) (if (currentSortAsc.get) sortThAsc else sortThDesc) else sortThNone) &
            "th [onclick]" #> clickedSortableHeader(this)
        else PassThru)
  }

  trait SortQuery extends Query {
    var sortColumn: C
    var sortAsc: Boolean
  }

  type C <: SortCol
  type Q <: SortQuery

  protected def defaultSortCol = columns.head
  protected var _currentSortColName: String = defaultSortCol.name
  protected def currentSortCol: C = columns.find(_.name == _currentSortColName).getOrElse(defaultSortCol)
  protected def currentSortCol_=(c: C): Unit = _currentSortColName = c.name
  protected val currentSortAsc: FatLazy[Boolean] = FatLazy(currentSortCol.defaultSortAsc)

  protected def clickedSortableHeader(col: C) = SHtml.onEvent(_ => {
    val before = currentSortCol

    if (col.name == currentSortCol.name) {
      currentSortAsc() = !currentSortAsc.get
    } else {
      currentSortCol = col
      currentSortAsc() = col.defaultSortAsc
    }

    Run(s"""$$('[col-table="${id('table)}"][col-name="${before.name}"]').removeClass('$sortThAsc $sortThDesc').addClass('$sortThNone')""") &
      Run(s"""$$('[col-table="${id('table)}"][col-name="${col.name}"]').removeClass('$sortThNone').addClass('${if (currentSortAsc.get) sortThAsc else sortThDesc}')""") &
      rerenderTableBody()
  })

  override protected def prepareQuery(_query: Q): Q = {
    val query = super.prepareQuery(_query)
    query.sortColumn = currentSortCol
    query.sortAsc = currentSortAsc.get
    query
  }
}
