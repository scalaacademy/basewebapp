package code.pages.theme

import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers

import scala.xml.NodeSeq

trait BWAGrid {

  object G {

    case class Span(ns: NodeSeq = NodeSeq.Empty, clas: String = "", visible: Boolean = true) {

      val id = Helpers.nextFuncName

      def show(): JsCmd = Run( s"""$$('#$id').show()""")
      def hide(): JsCmd = Run( s"""$$('#$id').hide()""")

      def withVisibility(visible: Boolean) = copy(visible = visible)
      def withClass(cls: String) = copy(clas = clas + " " + cls)

      def offset2 = copy(clas = clas + " offset2")
      def offset3 = copy(clas = clas + " offset3")
      def offset4 = copy(clas = clas + " offset4")
      def offset5 = copy(clas = clas + " offset5")
      def offset6 = copy(clas = clas + " offset6")
      def offset7 = copy(clas = clas + " offset7")
      def offset8 = copy(clas = clas + " offset8")
      def offset9 = copy(clas = clas + " offset9")
      def offset10 = copy(clas = clas + " offset10")
      def offset11 = copy(clas = clas + " offset11")
      def offset12 = copy(clas = clas + " offset12")

      def render = <div style={if (!visible) "display:none;" else null} id={id} class={clas}>{ns}</div>

      def apply(ns: NodeSeq*) = copy(ns = ns.reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty))
      def apply(ns1: Renderable, ns: Renderable*) = copy(ns = ns1.rendered ++ ns.map(_.rendered).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty))
    }

    type Renderable = {def rendered: NodeSeq}

    //    def row_fluid(ns1: Span, ns: Span*) = <div class="row-fluid">{(ns1.render +: ns.map(_.render)).reduce[NodeSeq](_ ++ _)}</div>
    def row_fluid(ns: Span*) = <div class="row-fluid">{ns.map(_.render).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty)}</div>

    def span2 = Span(clas = "span2")
    def span3 = Span(clas = "span3")
    def span4 = Span(clas = "span4")
    def span5 = Span(clas = "span5")
    def span6 = Span(clas = "span6")
    def span7 = Span(clas = "span7")
    def span8 = Span(clas = "span8")
    def span9 = Span(clas = "span9")
    def span10 = Span(clas = "span10")
    def span11 = Span(clas = "span11")
    def span12 = Span(clas = "span12")
  }

  object G3 {

    object C {
      def apply(ns: NodeSeq) = <div class="container-fluid">{ns}</div>
    }

    object Row {
      def apply(cols: Col*) = <div class="row">{cols.map(_.ns).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty)}</div>
      def apply(classes: String, cols: Col*) = <div class={"row " + classes}>{cols.map(_.ns).reduceOption[NodeSeq](_ ++ _).getOrElse(NodeSeq.Empty)}</div>
    }

    case class Col(clas: String = "", attrs: List[ElemAttr] = Nil, contents: NodeSeq = NodeSeq.Empty) {
      def ns = attrs.foldLeft(<div class={clas}>{contents}</div>)(_ % _)

      def apply(ns: NodeSeq) = copy(contents = ns)

      def xs1 = copy(clas = clas + " col-xs-1")
      def xs2 = copy(clas = clas + " col-xs-2")
      def xs3 = copy(clas = clas + " col-xs-3")
      def xs4 = copy(clas = clas + " col-xs-4")
      def xs5 = copy(clas = clas + " col-xs-5")
      def xs6 = copy(clas = clas + " col-xs-6")
      def xs7 = copy(clas = clas + " col-xs-7")
      def xs8 = copy(clas = clas + " col-xs-8")
      def xs9 = copy(clas = clas + " col-xs-9")
      def xs10 = copy(clas = clas + " col-xs-10")
      def xs11 = copy(clas = clas + " col-xs-11")
      def xs12 = copy(clas = clas + " col-xs-12")
      //
      def sm1 = copy(clas = clas + " col-sm-1")
      def sm2 = copy(clas = clas + " col-sm-2")
      def sm3 = copy(clas = clas + " col-sm-3")
      def sm4 = copy(clas = clas + " col-sm-4")
      def sm5 = copy(clas = clas + " col-sm-5")
      def sm6 = copy(clas = clas + " col-sm-6")
      def sm7 = copy(clas = clas + " col-sm-7")
      def sm8 = copy(clas = clas + " col-sm-8")
      def sm9 = copy(clas = clas + " col-sm-9")
      def sm10 = copy(clas = clas + " col-sm-10")
      def sm11 = copy(clas = clas + " col-sm-11")
      def sm12 = copy(clas = clas + " col-sm-12")
      //
      def md1 = copy(clas = clas + " col-md-1")
      def md2 = copy(clas = clas + " col-md-2")
      def md3 = copy(clas = clas + " col-md-3")
      def md4 = copy(clas = clas + " col-md-4")
      def md5 = copy(clas = clas + " col-md-5")
      def md6 = copy(clas = clas + " col-md-6")
      def md7 = copy(clas = clas + " col-md-7")
      def md8 = copy(clas = clas + " col-md-8")
      def md9 = copy(clas = clas + " col-md-9")
      def md10 = copy(clas = clas + " col-md-10")
      def md11 = copy(clas = clas + " col-md-11")
      def md12 = copy(clas = clas + " col-md-12")
      //
      def lg1 = copy(clas = clas + " col-lg-1")
      def lg2 = copy(clas = clas + " col-lg-2")
      def lg3 = copy(clas = clas + " col-lg-3")
      def lg4 = copy(clas = clas + " col-lg-4")
      def lg5 = copy(clas = clas + " col-lg-5")
      def lg6 = copy(clas = clas + " col-lg-6")
      def lg7 = copy(clas = clas + " col-lg-7")
      def lg8 = copy(clas = clas + " col-lg-8")
      def lg9 = copy(clas = clas + " col-lg-9")
      def lg10 = copy(clas = clas + " col-xlg10")
      def lg11 = copy(clas = clas + " col-xlg11")
      def lg12 = copy(clas = clas + " col-xlg12")

      //

      def xs_offset1 = copy(clas = clas + " col-xs-offset-1")
      def xs_offset2 = copy(clas = clas + " col-xs-offset-2")
      def xs_offset3 = copy(clas = clas + " col-xs-offset-3")
      def xs_offset4 = copy(clas = clas + " col-xs-offset-4")
      def xs_offset5 = copy(clas = clas + " col-xs-offset-5")
      def xs_offset6 = copy(clas = clas + " col-xs-offset-6")
      def xs_offset7 = copy(clas = clas + " col-xs-offset-7")
      def xs_offset8 = copy(clas = clas + " col-xs-offset-8")
      def xs_offset9 = copy(clas = clas + " col-xs-offset-9")
      def xs_offset10 = copy(clas = clas + " col-xs-offset-10")
      def xs_offset11 = copy(clas = clas + " col-xs-offset-11")
      def xs_offset12 = copy(clas = clas + " col-xs-offset-12")
      //
      def sm_offset1 = copy(clas = clas + " col-sm-offset-1")
      def sm_offset2 = copy(clas = clas + " col-sm-offset-2")
      def sm_offset3 = copy(clas = clas + " col-sm-offset-3")
      def sm_offset4 = copy(clas = clas + " col-sm-offset-4")
      def sm_offset5 = copy(clas = clas + " col-sm-offset-5")
      def sm_offset6 = copy(clas = clas + " col-sm-offset-6")
      def sm_offset7 = copy(clas = clas + " col-sm-offset-7")
      def sm_offset8 = copy(clas = clas + " col-sm-offset-8")
      def sm_offset9 = copy(clas = clas + " col-sm-offset-9")
      def sm_offset10 = copy(clas = clas + " col-sm-offset-10")
      def sm_offset11 = copy(clas = clas + " col-sm-offset-11")
      def sm_offset12 = copy(clas = clas + " col-sm-offset-12")
      //
      def md_offset1 = copy(clas = clas + " col-md-offset-1")
      def md_offset2 = copy(clas = clas + " col-md-offset-2")
      def md_offset3 = copy(clas = clas + " col-md-offset-3")
      def md_offset4 = copy(clas = clas + " col-md-offset-4")
      def md_offset5 = copy(clas = clas + " col-md-offset-5")
      def md_offset6 = copy(clas = clas + " col-md-offset-6")
      def md_offset7 = copy(clas = clas + " col-md-offset-7")
      def md_offset8 = copy(clas = clas + " col-md-offset-8")
      def md_offset9 = copy(clas = clas + " col-md-offset-9")
      def md_offset10 = copy(clas = clas + " col-md-offset-10")
      def md_offset11 = copy(clas = clas + " col-md-offset-11")
      def md_offset12 = copy(clas = clas + " col-md-offset-12")
      //
      def lg_offset1 = copy(clas = clas + " col-lg-offset-1")
      def lg_offset2 = copy(clas = clas + " col-lg-offset-2")
      def lg_offset3 = copy(clas = clas + " col-lg-offset-3")
      def lg_offset4 = copy(clas = clas + " col-lg-offset-4")
      def lg_offset5 = copy(clas = clas + " col-lg-offset-5")
      def lg_offset6 = copy(clas = clas + " col-lg-offset-6")
      def lg_offset7 = copy(clas = clas + " col-lg-offset-7")
      def lg_offset8 = copy(clas = clas + " col-lg-offset-8")
      def lg_offset9 = copy(clas = clas + " col-lg-offset-9")
      def lg_offset10 = copy(clas = clas + " col-xlg10-offset")
      def lg_offset11 = copy(clas = clas + " col-xlg11-offset")
      def lg_offset12 = copy(clas = clas + " col-xlg12-offset")
    }

  }

}