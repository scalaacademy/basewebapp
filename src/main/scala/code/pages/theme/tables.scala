package code.pages.theme

import java.util.Date
import code.pages.theme.ModalEditor
import com.github.david04.liftutils.elem.Validation.ReqStrVal
import com.github.david04.liftutils.elem._
import com.github.david04.liftutils.loc.{Loc, LocC, LocP}
import com.github.david04.liftutils.modtbl._
import com.github.david04.liftutils.util.LocalTime
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml.{NodeSeq, Text}

trait BWATables {
  self: BWAModals with BWAPortlets =>

  trait EditableHeaderTable extends com.github.david04.liftutils.modtbl.Table {
    selfT =>

    implicit def locP: LocP

    trait EditableHeaderCol extends TableCol {
      self: C =>

      override def thClasses: List[String] = "editable-header-col" :: super.thClasses

      def editableHeaderCol: Boolean

      def getEditableHeaderValue: String

      def setEditableHeaderValue(v: String): JsCmd

      def editHeaderOnClickClientSide(): JsCmd = SHtml.ajaxInvoke(() => {
        new ModalEditor() {

          override protected def p: Loc = locP

          override protected def onSave(): JsCmd = Noop

          override protected def buildElems(): Seq[HTMLViewableElem] =
            new DefaultEditableElems.Text("header", getEditableHeaderValue, setEditableHeaderValue _) with ReqStrVal :: Nil
        }.show()
      }).cmd

      def editableHeaderNs =
        <div onclick={s"""var event = arguments[0] || window.event; event.stopPropagation(); ${editHeaderOnClickClientSide().toJsCmd};"""} class="editable-header-col-btn"><i class="fa fa-pencil"></i></div>

      override def renderHead: NodeSeq => NodeSeq =
        super.renderHead andThen
          (if (editableHeaderCol) "th *+" #> editableHeaderNs else PassThru)
    }

    type C <: EditableHeaderCol

  }

  trait Table extends DefaultSimpleTable3
  with KnownSizePaginatedQueryableTable
  with EmptyKnownSizeTable {

    override type ColFactory = DefaultColFactory
    override type C = DefaultColumn

    def boxedTable: Boolean = false

    override protected def templatePath: List[String] = "templates-hidden" :: (if (boxedTable) "modtbl-dflt-boxed" else "modtbl-dflt-widget") :: Nil

    //    override protected def pagBtnsDisabledClass: String = "disabled"
    //    override protected def pagBtnsCurrentClass: String = "active"
    override protected lazy val pagBtnsCurrentClass = "paginate_active"
    override protected lazy val pagBtnsDisabledClass = "paginate_button_disabled"

    def fac(_defaultSortAsc: Boolean = true, center: Boolean = false, extraTdStyle: List[String] = Nil, literalName: Boolean = false) = new DefaultColFactory {
      override def build(_name: String, _renderRow: (R, String, Int, String, Int) => NodeSeq => NodeSeq, _sort: Option[SortFunc], _tdClasses: List[String], _tdStyle: List[String]): C =
        new DefaultColumn {
          override val sort: Option[SortFunc] = _sort

          override def title = if (literalName) name else super.title

          override def name: String = _name

          override def renderRow(row: R, rowId: String, rowIdx: Int, colId: String, colIdx: Int, rrdRow: () => JsCmd): (NodeSeq) => NodeSeq =
            super.renderRow(row, rowId, rowIdx, colId, colIdx, rrdRow) andThen
              _renderRow(row, rowId, rowIdx, colId, colIdx)

          override def defaultSortAsc: Boolean = _defaultSortAsc

          override def thStyle = if (center) "text-align:center;" :: super.thStyle else super.thStyle

          override def tdStyle = (if (center) List("text-align:center;") else Nil) ::: extraTdStyle ::: _tdStyle ::: super.tdStyle

          override def tdClasses: List[String] = _tdClasses ::: super.tdClasses
        }
    }

    implicit lazy val defaultFac: ColFactory = fac()

    override def hidePaginationForOnePage: Boolean = true

    def tableLight: Boolean = true

    def tableHover: Boolean = true

    def smallWidth: Boolean = false

    override def templateTransforms(): NodeSeq => NodeSeq =
      super.templateTransforms() andThen {
        ".modtbl-table [class+]" #> (
          (if (tableLight) " table-light " else "") +
            (if (tableHover) " table-hover " else "")
          ) //&
        //          (if (smallWidth) {
        //            ".modtbl-pag-info-around [class+]" #> "col-md-12" &
        //              ".modtbl-pag-btns-around [class+]" #> "col-md-12"
        //          } else {
        //            ".modtbl-pag-info-around [class+]" #> "col-md-5 col-sm-12" &
        //              ".modtbl-pag-btns-around [class+]" #> "col-md-7 col-sm-12"
        //          })
      }
  }

  trait SimpleTable extends Table with LocC {
    override protected def n: String = "table"

    override type SortFunc = Seq[R] => Seq[R]

    def all: Seq[R]

    override protected def rowsSize: Long = all.size

    override protected def query(params: Q): Seq[R] = {
      val sorted1 = params.sortColumn.sort.map(_(all)).getOrElse(all)
      val sorted2 = if (params.sortAsc) sorted1 else sorted1.reverse
      sorted2.drop(params.pageOffset).take(params.pageSize)
    }

    def ColLocalTimeEdit(name: String, get: R => Option[Long], set: (R, Option[Long]) => JsCmd, sort: Option[SortFunc] = None, allowNone: Boolean = false)(implicit fac: ColFactory): C = {
      val fmt: String = "Do MMM YYYY, h:mm a ZZ"

      ColSimpleNsFactory(name, r => {
        var editing = false
        SHtml.idMemoize(renderer => (_: NodeSeq) => {
          if (!editing) {
          <span onclick={SHtml.ajaxInvoke(() => {editing = true; renderer.setHtml()}).toJsCmd}>{get(r).map(date => {
            LocalTime.localTime(new Date(date))
          }).getOrElse(Text("[empty]"))}</span>
        } else {
            val id = Helpers.nextFuncName
            val hide = Run(s"$$('#$id').datetimepicker('hide');")

            val setDate = SHtml.ajaxCall(JsRaw("new Date(e.date.setTime(e.date.getTime() + (e.date.getTimezoneOffset() * 60000))).getTime()"),
              (s: String) => {set(r, Some(s.toLong)); editing = false; hide & renderer.setHtml()}).toJsCmd
            val clearDate = SHtml.ajaxInvoke(() => {set(r, None); editing = false; hide & renderer.setHtml()}).toJsCmd

            <input type="text" id={id} class="edit-date"></input> ++
            <tail>{Script(OnLoad(Run(
              s"""$$('#$id').datetimepicker({format: "dd/mm/yyyy hh:ii"})
                 .on('changeDate', function(e){if(e.date) {$setDate} else if ($allowNone) {$clearDate}});
                 ${get(r).map(date => s"$$('#$id').datetimepicker('setDate', new Date(${date}));").getOrElse("")}
                 $$('#$id').datetimepicker('show');"""
            )))}</tail>
          }
        }).apply(<span></span>)
      }, sort)(fac)
    }
  }

  trait PortletTable extends Portlet with Table with LocC {
    override protected def n: String = "table"

    override def portletWidgetBlock: Boolean = false

    lazy val selectedColumns: collection.mutable.Set[C] = collection.mutable.Set(columns: _*)

    override protected def showColumn(c: DefaultColumn): Boolean = selectedColumns.contains(c)

    override def portletActions: NodeSeq =
      new TH.BtnGroup(
        columns.map(col => {
          lazy val btn: TH.BtnSpan =
            new TH.BtnClas(cls = "")
              .lbl(col.title)
              .icn(if (selectedColumns.contains(col)) TH.IcnFA.icn_check_square_o else TH.IcnFA.icn_square_o)
              .ajax({
              synchronized {
                if (selectedColumns.contains(col) && selectedColumns.size > 1) selectedColumns -= col
                else selectedColumns += col
                (btn.icn() = if (selectedColumns.contains(col)) TH.IcnFA.icn_check_square_o else TH.IcnFA.icn_square_o) &
                  rerenderTable()
              }
            })

          btn.btnLink
        }): _*
      )(TH.IcnLedGray.application_columns_co).rendered

    override def portletContents: NodeSeq = renderedTable()
  }

}