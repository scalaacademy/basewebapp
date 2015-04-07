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

import net.liftweb.common.Box
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.{SHtml, SHtml2}
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearNodes, PassThru}

import scala.xml.NodeSeq


trait PaginatedQueryableTable extends QueryableTable {

  trait PagQuery extends Query {
    var pageSize: Int
    var pageOffset: Int
  }

  type Q <: PagQuery

  override def keepClasses: List[String] = "modtbl-pag-btns-around" :: super.keepClasses

  protected def pagBtnsCurrentClass: String
  protected def pagBtnsDisabledClass: String
  protected def pagBtnsShowFirstAndLast: Boolean = true
  protected def pagShowPagInfo: Boolean = true
  protected lazy val pagNBtns = 5
  protected def pagPageSize = 40

  def hidePaginationForOnePage: Boolean = false

  protected var currentPage = 0
  protected def pageSizes = 10 :: 20 :: 40 :: 60 :: 100 :: Nil
  protected var pageSize = pagPageSize

  override protected def prepareQuery(_query: Q): Q = {
    val query = super.prepareQuery(_query)
    query.pageSize = pageSize
    query.pageOffset = currentPage * pageSize
    query
  }

  override protected def pageTransforms(rowsSizeOpt: Option[Long]) =
    super.pageTransforms(rowsSizeOpt) andThen
      ".modtbl-pag-btns-around" #> paginationButtonsRenderer &
        ".modtbl-pag-info-around" #> (if (pagShowPagInfo) paginationInfoRenderer(rowsSizeOpt) else ClearNodes) &
        ".modtbl-page-size" #> SHtml.ajaxSelectElem[Int](pageSizes, Box(pageSizes.find(_ == pageSize)))(s => {
          pageSize = s
          rerenderPage()
        })

  protected def firstPage() = SHtml.onEvent(_ => {
    currentPage = 0
    rerenderPage()
  }).cmd & Run("return false;")

  protected def prevPage() = SHtml.onEvent(_ => {
    currentPage = math.max(0, currentPage - 1)
    rerenderPage()
  }).cmd & Run("return false;")

  protected def toPage(n: Int) = SHtml.onEvent(_ => {
    currentPage = n
    rerenderPage()
  }).cmd & Run("return false;")

  protected def nextPage(rowsSize: Long) = SHtml.onEvent(_ => {
    currentPage = math.min(nPages(rowsSize) - 1, currentPage + 1)
    rerenderPage()
  }).cmd & Run("return false;")

  protected def lastPage(rowsSize: Long) = SHtml.onEvent(_ => {
    currentPage = nPages(rowsSize) - 1
    rerenderPage()
  }).cmd & Run("return false;")

  protected def currentButtons(rowsSize: Long) = {
    val side = pagNBtns - 1
    val all = ((currentPage - side) until currentPage) ++ ((currentPage + 1) to (currentPage + side))
    all.filter(_ >= 0).filter(_ < nPages(rowsSize)).sortBy(n => math.abs(currentPage - n)).take(side).:+(currentPage).sorted
  }

  protected lazy val paginationButtonsRenderer = SHtml.idMemoize(_ => paginationButtonsTransforms(rowsSizeOpt))

  protected def paginationButtonsTransforms(rowsSizeOpt: Option[Long]): NodeSeq => NodeSeq = {
    val rowsSize = rowsSizeOpt.getOrElse(Long.MaxValue / 2)
    if (nPages(rowsSize) == 1 && hidePaginationForOnePage) ClearNodes
    else {
      (if (!pagBtnsShowFirstAndLast) ".modtbl-pag-first" #> ClearNodes & ".modtbl-pag-last" #> ClearNodes else PassThru) andThen
        ".modtbl-pag-first [class+]" #> (if (currentPage == 0) pagBtnsDisabledClass else "") &
          ".modtbl-pag-first" #> {".modtbl-pag-btn [onclick]" #> firstPage()} &
          ".modtbl-pag-prev [class+]" #> (if (currentPage == 0) pagBtnsDisabledClass else "") &
          ".modtbl-pag-prev" #> {".modtbl-pag-btn [onclick]" #> prevPage()} &
          ".modtbl-pag-next [class+]" #> (if (currentPage == nPages(rowsSize) - 1) pagBtnsDisabledClass else "") &
          ".modtbl-pag-next" #> {".modtbl-pag-btn [onclick]" #> nextPage(rowsSize)} &
          ".modtbl-pag-last [class+]" #> (if (currentPage == nPages(rowsSize) - 1) pagBtnsDisabledClass else "") &
          ".modtbl-pag-last" #> {".modtbl-pag-btn [onclick]" #> lastPage(rowsSize)} andThen
        ".modtbl-pag-n" #> currentButtons(rowsSize).map(n =>
          ".modtbl-pag-n [class+]" #> (if (currentPage == n) pagBtnsCurrentClass else "") &
            ".modtbl-pag-btn *" #> (n + 1).toString &
            ".modtbl-pag-btn [onclick]" #> toPage(n))
    }
  }

  protected lazy val paginationInfoRenderer = SHtml2.idMemoize1[Option[Long]]((_, rowsSizeOpt) => paginationInfoTransforms(rowsSizeOpt))

  override def rerenderTable(): JsCmd = super.rerenderTable() & paginationInfoRenderer.setHtml(rowsSizeOpt) & paginationButtonsRenderer.setHtml()

  override def rerenderTableBody(): JsCmd = super.rerenderTableBody() & paginationInfoRenderer.setHtml(rowsSizeOpt) & paginationButtonsRenderer.setHtml()

  protected def nPages(rowsSize: Long) = math.max(1, math.ceil(rowsSize / pageSize.toDouble).toInt)

  protected def paginationInfoTransforms(rowsSize: Option[Long]): NodeSeq => NodeSeq =
    ".modtbl-pag-info [id]" #> id('modtbl_pag_info) &
      ".modtbl-pag-info *" #> rowsSize.map(rowsSize => {
        loc("pagInfo",
          "from" -> math.min(rowsSize, (currentPage * pageSize + 1)).toString,
          "to" -> math.min(rowsSize, ((currentPage + 1) * pageSize)).toString,
          "total" -> rowsSize.toString)
      }).getOrElse("--")
}
