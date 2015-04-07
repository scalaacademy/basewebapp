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

import java.util.Date

import com.github.david04.liftutils.util.InPlace
import net.liftweb.http.js.JsCmds.{OnLoad, Run, Script}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.util.Try
import scala.xml.NodeSeq

abstract class DefaultTable extends Table
                                    with NamedTable
                                    with QueryableTable
                                    with PaginatedQueryableTable
                                    with SortableQueryableTable
                                    with LocStrHeadTable
                                    with StrRowTable
                                    with ZebraTable {
  type Q = DefaultQuery
  type C <: DefaultColumn

  case class DefaultQuery(
                           var pageSize: Int,
                           var pageOffset: Int,
                           var sortColumn: C,
                           var sortAsc: Boolean) extends Query
                                                         with PagQuery
                                                         with SortQuery

  def createQuery = DefaultQuery(0, 0, columns.head, false)

  abstract class DefaultColumn(
                                name: String,
                                rowValue: R => String) extends TableCol
                                                               with LocStrHeadCol
                                                               with StrRowCol
                                                               with SortCol {
    self: C =>

    def rowStrValue: R => String = rowValue
  }

}

trait DefaultSimpleTable2 extends Table
                                  with NamedTable
                                  with QueryableTable
                                  with PaginatedQueryableTable
                                  with SortableQueryableTable
                                  with LocStrHeadTable
                                  with StrRowTable
                                  with ZebraTable {

  override protected def templatePath: List[String] = "templates-hidden" :: "modtbl-simple" :: Nil

  case class DefaultQuery(
                           var pageSize: Int,
                           var pageOffset: Int,
                           var sortColumn: C,
                           var sortAsc: Boolean) extends Query with PagQuery with SortQuery

  def createQuery = DefaultQuery(0, 0, columns.head, false)

  trait DefaultColumn extends TableCol
                              with LocStrHeadCol
                              with SortCol {
    val sort: Option[SortFunc]
    def sortable = sort.isDefined
  }

  type Q = DefaultQuery
  type C = DefaultColumn
  type SortFunc

  case class DefaultStrCol private(
                                    name: String,
                                    rowValue: R => String,
                                    sort: Option[SortFunc]
                                    ) extends DefaultColumn with StrRowCol {

    def this(name: String, rowValue: R => String, sort: SortFunc) = this(name, rowValue, Some(sort))
    def this(name: String, rowValue: R => String) = this(name, rowValue, None)

    def rowStrValue: R => String = rowValue
  }

  case class DefaultLocalTimeCol private(
                                          name: String,
                                          rowValue: R => Date,
                                          sort: Option[SortFunc]
                                          ) extends DefaultColumn with NodeSeqRowCol {

    def fmt: String = "Do MMM YYYY, h:mm a ZZ"

    def this(name: String, rowValue: R => Date, sort: SortFunc) = this(name, rowValue, Some(sort))
    def this(name: String, rowValue: R => Date) = this(name, rowValue, None)

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      val id = Helpers.nextFuncName
      <span id={id}></span> ++ <tail>{Script(OnLoad(Run(
        s"""$$('#$id').text(moment(${rowValue(r).getTime}).format(${fmt.encJs}));""".stripMargin
      )))}</tail>
    }
  }

  case class DefaultLocalTimeLeftCol private(
                                              name: String,
                                              rowValue: R => Date,
                                              sort: Option[SortFunc]
                                              ) extends DefaultColumn with NodeSeqRowCol {

    def this(name: String, rowValue: R => Date, sort: SortFunc) = this(name, rowValue, Some(sort))
    def this(name: String, rowValue: R => Date) = this(name, rowValue, None)

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      val id = Helpers.nextFuncName
      <span id={id}></span> ++ <tail>{Script(OnLoad(Run(
        s"""setInterval(function() {$$('#$id').text(moment(${rowValue(r).getTime}).fromNow());}, 800);""".stripMargin
      )))}</tail>
    }
  }

  case class DefaultStrEditCol private(
                                        name: String,
                                        get: R => String,
                                        set: (R, String) => JsCmd,
                                        sort: Option[SortFunc],
                                        allowEmpty: Boolean
                                        ) extends DefaultColumn with StrRowCol {

    def this(name: String, get: R => String, set: (R, String) => JsCmd, sort: SortFunc, allowEmpty: Boolean = true) =
      this(name, get, set, Some(sort), allowEmpty)

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.str(get(r), s => if (allowEmpty || s != "") set(r, s) else JsCmds.Noop, "[empty]")
    }
    def rowStrValue: R => String = get
  }

  case class DefaultOptStrEditCol private(
                                           name: String,
                                           get: R => Option[String],
                                           set: (R, Option[String]) => JsCmd,
                                           sort: Option[SortFunc],
                                           allowEmpty: Boolean
                                           ) extends DefaultColumn with StrRowCol {

    def this(
              name: String,
              get: R => Option[String],
              set: (R, Option[String]) => JsCmd,
              sort: SortFunc,
              allowEmpty: Boolean = true) = this(name, get, set, Some(sort), allowEmpty)

    def this(
              name: String,
              get: R => Option[String],
              set: (R, Option[String]) => JsCmd,
              allowEmpty: Boolean) = this(name, get, set, None, allowEmpty)

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.strOpt(get(r), s => if (allowEmpty || s.isDefined) set(r, s) else JsCmds.Noop, "[empty]")
    }
    def rowStrValue: R => String = _ => ""
  }

  case class DefaultOptIntEditCol private(
                                           name: String,
                                           get: R => Option[Int],
                                           set: (R, Option[Int]) => JsCmd,
                                           sort: Option[SortFunc],
                                           allowEmpty: Boolean
                                           ) extends DefaultColumn with StrRowCol {

    def this(name: String, get: R => Option[Int], set: (R, Option[Int]) => JsCmd, sort: SortFunc, allowEmpty: Boolean = true) =
      this(name, get, set, Some(sort), allowEmpty)

    def this(name: String, get: R => Option[Int], set: (R, Option[Int]) => JsCmd, allowEmpty: Boolean) =
      this(name, get, set, None, allowEmpty)

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.strOpt(get(r).map(_.toString), s => if (allowEmpty || s.isDefined) Try(set(r, s.map(_.toInt))).getOrElse(JsCmds.Noop) else JsCmds.Noop, "[empty]")
    }
    def rowStrValue: R => String = _ => ""
  }

  case class DefaultIntEditCol private(
                                        name: String,
                                        get: R => Int,
                                        set: (R, Int) => JsCmd,
                                        sort: Option[SortFunc]
                                        ) extends DefaultColumn with StrRowCol {

    def this(name: String, get: R => Int, set: (R, Int) => JsCmd, sort: SortFunc) = this(name, get, set, Some(sort))

    def this(name: String, get: R => Int, set: (R, Int) => JsCmd) = this(name, get, set, None)

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.str(get(r).toString, s => Try(set(r, s.toInt)).getOrElse(JsCmds.Noop), "[empty]")
    }
    def rowStrValue: R => String = _ => ""
  }

  case class DefaultOptDoubleEditCol private(
                                              name: String,
                                              get: R => Option[Double],
                                              set: (R, Option[Double]) => JsCmd,
                                              sort: Option[SortFunc],
                                              allowEmpty: Boolean
                                              ) extends DefaultColumn with StrRowCol {

    def this(name: String, get: R => Option[Double], set: (R, Option[Double]) => JsCmd, sort: SortFunc, allowEmpty: Boolean = true) =
      this(name, get, set, Some(sort), allowEmpty)

    def this(name: String, get: R => Option[Double], set: (R, Option[Double]) => JsCmd, allowEmpty: Boolean) =
      this(name, get, set, None, allowEmpty)

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.strOpt(get(r).map(_.toString), s => if (allowEmpty || s.isDefined) Try(set(r, s.map(_.toDouble))).getOrElse(JsCmds.Noop) else JsCmds.Noop, "[empty]")
    }
    def rowStrValue: R => String = _ => ""
  }

  case class DefaultPasswordEditCol private(
                                             name: String,
                                             set: (R, String) => JsCmd,
                                             sort: Option[SortFunc]
                                             ) extends DefaultColumn with StrRowCol {

    def this(name: String, set: (R, String) => JsCmd, sort: SortFunc) = this(name, set, Some(sort))

    override def tdClasses = "editable-col" :: super.tdClasses
    override def tdStyle = "position:relative" :: super.tdStyle

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = {
      InPlace.str("", s => set(r, s), "Type to change")
    }
    def rowStrValue: R => String = _ => ""
  }

  case class DefaultNSColumn private(
                                      name: String,
                                      rowValue: R => NodeSeq,
                                      sort: Option[SortFunc]
                                      ) extends DefaultColumn with NodeSeqRowCol {
    self: C =>

    def this(name: String, rowValue: R => NodeSeq, sort: SortFunc) = this(name, rowValue, Some(sort))
    def this(name: String, rowValue: R => NodeSeq) = this(name, rowValue, None)

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = rowValue(r)
  }

}

trait DefaultSimpleTable3 extends Table
                                  with NamedTable
                                  with QueryableTable
                                  with PaginatedQueryableTable
                                  with SortableQueryableTable
                                  with LocStrHeadTable
                                  with StrRowTable
                                  with TextAlignTable
                                  with ZebraTable {

  override protected def templatePath: List[String] = "templates-hidden" :: "modtbl-simple" :: Nil

  case class DefaultQuery(
                           var pageSize: Int,
                           var pageOffset: Int,
                           var sortColumn: C,
                           var sortAsc: Boolean) extends Query with PagQuery with SortQuery

  def createQuery = DefaultQuery(0, 0, columns.head, false)

  trait DefaultColumn extends TableCol
                              with LocStrHeadCol
                              with SortCol {
    self: C =>
    val sort: Option[SortFunc]
    def sortable = sort.isDefined
  }

  trait DefaultColFactory {
    def build(name: String, renderRow: (R, String, Int, String, Int) => NodeSeq => NodeSeq, sort: Option[SortFunc], tdClasses: List[String], tdStyle: List[String]): C
  }

  type Q = DefaultQuery
  type C <: DefaultColumn
  type SortFunc
  type ColFactory <: DefaultColFactory

  def ColSimpleNsFactory(name: String, rowValue: R => NodeSeq, sort: Option[SortFunc], tdClasses: List[String] = Nil, tdStyle: List[String] = Nil)(implicit fac: ColFactory): C = fac.build(name, (r, _, _, _, _) => "td *" #> rowValue(r), sort, Nil, Nil)

  def ColNs(name: String, rowValue: R => NodeSeq, sort: SortFunc)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, rowValue, Some(sort))
  def ColNs(name: String, rowValue: R => NodeSeq)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, rowValue, None)
  def ColSpan(name: String, rowValue: R => String, clas: R => String, onclick: R => JsCmd = _ => JsCmds.Noop)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, r => <span onclick={onclick(r).toJsCmd} class={clas(r)}>{rowValue(r)}</span>, None)
  def ColBtns(name: String, btns: (R => String, R => String, R => JsCmd)*)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, r => btns.map(btn => <span onclick={btn._3(r).toJsCmd} class={btn._2(r)}>{btn._1(r)}</span>), None)
  def ColLink(name: String, rowValue: R => String, href: R => String, clas: String)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, r => <a class={clas} href={href(r)}>{rowValue(r)}</a>, None)
  def ColStr(name: String, rowValue: R => String, sort: SortFunc)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, r => scala.xml.Text(rowValue(r)), Some(sort))
  def ColStr(name: String, rowValue: R => String)(implicit fac: ColFactory): C = ColSimpleNsFactory(name, r => scala.xml.Text(rowValue(r)), None)
  def ColLocalTime(name: String, rowValue: R => Long, sort: Option[SortFunc] = None)(implicit fac: ColFactory): C = {
    val fmt: String = "Do MMM YYYY, h:mm a ZZ"

    ColSimpleNsFactory(name, r => {
      val id = Helpers.nextFuncName
      <span id={id}></span> ++ <tail>{Script(OnLoad(Run(
        s"""$$('#$id').text(moment(${rowValue(r)}).format(${fmt.encJs}));""".stripMargin
      )))}</tail>
    }, None)
  }
  def ColLocalTimeLeft(name: String, rowValue: R => Long, sort: Option[SortFunc])(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(name, r => {
      val id = Helpers.nextFuncName
      <span id={id}></span> ++ <tail>{Script(OnLoad(Run(
        s"""setInterval(function() {$$('#$id').text(moment(${rowValue(r)}).fromNow());}, 800);""".stripMargin
      )))}</tail>
    }, None)
  }
  def ColStrEdit(
                  name: String,
                  get: R => String,
                  set: (R, String) => JsCmd,
                  sort: Option[SortFunc],
                  allowEmpty: Boolean
                  )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(name,
      r => InPlace.str(get(r), s => if (allowEmpty || s != "") set(r, s) else JsCmds.Noop, "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColOptStrEdit(
                     name: String,
                     get: R => Option[String],
                     set: (R, Option[String]) => JsCmd,
                     sort: Option[SortFunc],
                     allowEmpty: Boolean
                     )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.strOpt(get(r), s => if (allowEmpty || s.isDefined) set(r, s) else JsCmds.Noop, "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColOptIntEdit(
                     name: String,
                     get: R => Option[Int],
                     set: (R, Option[Int]) => JsCmd,
                     sort: Option[SortFunc],
                     allowEmpty: Boolean
                     )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.strOpt(get(r).map(_.toString), s => if (allowEmpty || s.isDefined) Try(set(r, s.map(_.toInt))).getOrElse(JsCmds.Noop) else JsCmds.Noop, "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColIntEdit(
                  name: String,
                  get: R => Int,
                  set: (R, Int) => JsCmd,
                  sort: Option[SortFunc]
                  )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.str(get(r).toString, s => Try(set(r, s.toInt)).getOrElse(JsCmds.Noop), "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColLongEdit(
                   name: String,
                   get: R => Long,
                   set: (R, Long) => JsCmd,
                   sort: Option[SortFunc]
                   )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.str(get(r).toString, s => Try(set(r, s.toLong)).getOrElse(JsCmds.Noop), "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColOptDoubleEdit(
                        name: String,
                        get: R => Option[Double],
                        set: (R, Option[Double]) => JsCmd,
                        sort: Option[SortFunc],
                        allowEmpty: Boolean
                        )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.strOpt(get(r).map(_.toString), s => if (allowEmpty || s.isDefined) Try(set(r, s.map(_.toDouble))).getOrElse(JsCmds.Noop) else JsCmds.Noop, "[empty]"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
  def ColPasswordEdit(
                       name: String,
                       set: (R, String) => JsCmd,
                       sort: Option[SortFunc]
                       )(implicit fac: ColFactory): C = {
    ColSimpleNsFactory(
      name,
      r => InPlace.str("", s => set(r, s), "Type to change"),
      None,
      tdClasses = "editable-col" :: Nil,
      tdStyle = "position:relative" :: Nil
    )
  }
}

abstract class DefaultSimpleTable extends Table
                                          with NamedTable
                                          with LocStrHeadTable
                                          with StrRowTable {
  override protected def templatePath: List[String] = "templates-hidden" :: "modtbl-simple" :: Nil

  trait DefaultColumn extends TableCol with LocStrHeadCol {}

  type C = DefaultColumn

  case class DefaultStringColumn(
                                  name: String,
                                  rowValue: R => String) extends DefaultColumn
                                                                 with StrRowCol {
    self: C =>

    def rowStrValue: R => String = rowValue
  }

  case class DefaultNodeSeqColumn(
                                   name: String,
                                   rowValue: R => NodeSeq) extends DefaultColumn
                                                                   with NodeSeqRowCol {
    self: C =>

    override def rowNodeSeqValue(r: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): NodeSeq = rowValue(r)
  }

}

abstract class DefaultOpenableTable extends Table
                                            with NamedTable
                                            with QueryableTable
                                            with PaginatedQueryableTable
                                            with SortableQueryableTable
                                            with RowDetailsTable
                                            with LocStrHeadTable
                                            with StrRowTable
                                            with ZebraTable {
  type Q = DefaultQuery
  type C <: DefaultColumn

  case class DefaultQuery(
                           var pageSize: Int,
                           var pageOffset: Int,
                           var sortColumn: C,
                           var sortAsc: Boolean) extends Query
                                                         with PagQuery
                                                         with SortQuery

  def createQuery = DefaultQuery(0, 0, columns.head, false)

  abstract class DefaultColumn(
                                name: String,
                                rowValue: R => String) extends TableCol
                                                               with ClickableRowCol
                                                               with LocStrHeadCol
                                                               with StrRowCol
                                                               with SortCol {
    self: C =>
  }

}







