package code.pages.theme

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{GUIDJsExp, SHtml}
import net.liftweb.util.Helpers

import scala.xml.{Attribute, NodeSeq, Unparsed, UnprefixedAttribute}

trait BWAButtons extends BWAIcons {

  trait BtnTransforms[T <: BtnTransforms[T]] {
    self: T =>

    def id: String
    def withClass(cls: String): T
    def withStyle(style: String): T

    def xs: T = withClass(" btn-xs ")
    def sm: T = withClass(" btn-small ").withStyle(";line-height: 14px;padding: 2px 4px;")
    def lg: T = withClass(" btn-lg ")

    def pullRight: T = withClass(" pull-right ")
    def disabled: T = withClass(" disabled ")

    def changeToDefault = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend');""")
    def changeToPrimary = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-primary');""")
    def changeToInfo = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-info');""")
    def changeToSuccess = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-success');""")
    def changeToWarning = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-warning');""")
    def changeToDanger = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-danger');""")
    def changeToInverse = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-inverse');""")
    def changeToExtend = Run( s"""$$('#$id').removeClass('btn-primary btn-info btn-success btn-warning btn-danger btn-inverse btn-extend').addClass('btn-extend');""")

    def enable = Run( s"""$$('#$id').removeClass('disabled').removeAttr('disabled');""")
    def disable = Run( s"""$$('#$id').addClass('disabled').attr('disabled', 'true');""")
  }

  trait BtnTransforms2[T <: BtnTransforms[T]] extends (NodeSeq => NodeSeq) with BtnTransforms[T] {
    self: T =>

    def withAttr(attr: (String, String)): T
  }

  trait BtnRenderable {def rendered: NodeSeq}

  case class BtnClas(id: String = Helpers.nextFuncName, cls: String, style: String = "", attrs: List[(String, String)] = Nil) extends BtnTransforms[BtnClas] {

    def withClass(cls: String): BtnClas = copy(cls = this.cls + " " + cls)
    def withStyle(style: String): BtnClas = copy(style = this.style + ";" + style)

    def href(link: String): BtnLink = BtnLink(id = id, cls = cls, href = link, style = style, attrs = attrs)
    def onclick(js: JsCmd): BtnSpan = BtnSpan(id = id, cls = cls, onclick = Some(js), style = style, attrs = attrs)
    def onclick(js: GUIDJsExp): BtnSpan = onclick(js.cmd)
    def ajax(js: => JsCmd): BtnSpan = onclick(SHtml.ajaxInvoke(() => js))
    def lbl(lbl: String) = BtnLbl(id = id, cls = cls, lbl = scala.xml.Text(lbl), style = style, attrs = attrs)
    def icn(icn: Icn) = BtnLbl(id = id, cls = cls, lbl = icn.i, style = style, attrs = attrs)

    def tipTop(txt: String) = BtnLbl(id = id, cls = cls + " tip-top ", tipTop = txt, style = style, attrs = attrs)
    def tipRight(txt: String) = BtnLbl(id = id, cls = cls + " tip-left ", tipTop = txt, style = style, attrs = attrs)
    def tipLeft(txt: String) = BtnLbl(id = id, cls = cls + " tip-right ", tipTop = txt, style = style, attrs = attrs)
    def tipBottom(txt: String) = BtnLbl(id = id, cls = cls + " tip-bottom ", tipTop = txt, style = style, attrs = attrs)
  }

  case class BtnLbl(id: String = Helpers.nextFuncName, cls: String, lbl: NodeSeq = NodeSeq.Empty, tipTop: String = "", style: String = "", attrs: List[(String, String)] = Nil, icnOpt: Option[Icn] = None) extends BtnTransforms2[BtnLbl] with BtnRenderable {

    def ns: NodeSeq = attrs.foldLeft(<span id={id} style={style} class={cls}>{icnOpt.map(icn => icn.i ++ Unparsed("&nbsp;")).getOrElse(NodeSeq.Empty) ++ lbl}</span>)((lst, attr) => lst % Attribute(None, attr._1, scala.xml.Text(attr._2), null))

    def withClass(cls: String): BtnLbl = copy(cls = this.cls + " " + cls)
    def withStyle(style: String): BtnLbl = copy(style = this.style + ";" + style)
    def withAttr(attr: (String, String)): BtnLbl = copy(attrs = attr :: attrs)

    def onclick(js: JsCmd): BtnSpan = BtnSpan(id = id, cls = cls, lbl = lbl, tipTop = tipTop, onclick = Some(js), style = style, attrs = attrs, icnOpt = icnOpt)
    def onclick(js: GUIDJsExp): BtnSpan = onclick(js.cmd)
    def ajax(js: => JsCmd): BtnSpan = onclick(SHtml.ajaxInvoke(() => js))
    def href(link: String) = BtnLink(id = id, cls = cls, lbl = lbl, tipTop = tipTop, href = link, style = style, attrs = attrs)
    def icn(icn: Icn) = copy(icnOpt = Some(icn))

    def icn = new {
      def update(icn: Icn): JsCmd = SetHtml(id, icn.i ++ Unparsed("&nbsp;") ++ lbl)
    }

    def tipTop(txt: String) = copy(cls = cls + " tip-top ", tipTop = txt)
    def tipRight(txt: String) = copy(cls = cls + " tip-left ", tipTop = txt)
    def tipLeft(txt: String) = copy(cls = cls + " tip-right ", tipTop = txt)
    def tipBottom(txt: String) = copy(cls = cls + " tip-bottom ", tipTop = txt)

    def apply(ns: NodeSeq) = this.ns

    def rendered = this.ns
  }

  case class BtnSpan(id: String = Helpers.nextFuncName, cls: String, lbl: NodeSeq = NodeSeq.Empty, tipTop: String = "", onclick: Option[JsCmd] = None, style: String = "", attrs: List[(String, String)] = Nil, icnOpt: Option[Icn] = None) extends BtnTransforms2[BtnSpan] with BtnRenderable {

    def btn: NodeSeq = attrs.foldLeft(<button data-original-title={if (tipTop != "") tipTop else null} id={id} style={style} onclick={onclick.map(_.toJsCmd).getOrElse(null)} class={cls}>{icnOpt.map(icn => icn.i ++ Unparsed("&nbsp;")).getOrElse(NodeSeq.Empty) ++ lbl}</button>)((lst, attr) => lst % new UnprefixedAttribute(attr._1, scala.xml.Text(attr._2), null))
    def btnLink: NodeSeq = attrs.foldLeft(<a data-original-title={if (tipTop != "") tipTop else null} id={id} style={style} href="javascript:void(0)" onclick={onclick.map(_.toJsCmd).getOrElse(null)} class={cls}>{icnOpt.map(icn => icn.i ++ Unparsed("&nbsp;")).getOrElse(NodeSeq.Empty) ++ lbl}</a>)((lst, attr) => lst % new UnprefixedAttribute(attr._1, scala.xml.Text(attr._2), null))

    def withClass(cls: String): BtnSpan = copy(cls = this.cls + " " + cls)
    def withStyle(style: String): BtnSpan = copy(style = this.style + ";" + style)
    def withAttr(attr: (String, String)): BtnSpan = copy(attrs = attr :: attrs)

    def icn = new {
      def update(icn: Icn): JsCmd = SetHtml(id, icn.i ++ Unparsed("&nbsp;") ++ lbl)
    }

    def onclick(js: JsCmd): BtnSpan = copy(onclick = Some(js))
    def onclick(js: GUIDJsExp): BtnSpan = onclick(js.cmd)
    def ajax(js: => JsCmd): BtnSpan = onclick(SHtml.ajaxInvoke(() => js))

    def tipTop(txt: String) = copy(cls = cls + " tip-top ", tipTop = txt)
    def tipRight(txt: String) = copy(cls = cls + " tip-left ", tipTop = txt)
    def tipLeft(txt: String) = copy(cls = cls + " tip-right ", tipTop = txt)
    def tipBottom(txt: String) = copy(cls = cls + " tip-bottom ", tipTop = txt)

    def apply(ns: NodeSeq) = this.btn

    def rendered = this.btn
  }

  case class BtnLink(
                      id: String = Helpers.nextFuncName,
                      cls: String,
                      lbl: NodeSeq = NodeSeq.Empty,
                      extraNs: NodeSeq = NodeSeq.Empty,
                      tipTop: String = "",
                      href: String = "javascript:void(0)",
                      style: String = "",
                      attrs: List[(String, String)] = Nil
                      ) extends BtnTransforms2[BtnLink] with BtnRenderable {

    def ns: NodeSeq = extraNs ++ attrs.foldLeft(<a data-original-title={if (tipTop != "") tipTop else null} id={id} style={style} href={href} class={cls}>{lbl}</a>)((lst, attr) => lst % Attribute(None, attr._1, scala.xml.Text(attr._2), null))

    def withExtraNs(ns: NodeSeq): BtnLink = copy(extraNs = this.extraNs ++ ns)
    def withClass(cls: String): BtnLink = copy(cls = this.cls + " " + cls)
    def withStyle(style: String): BtnLink = copy(style = this.style + ";" + style)
    def withAttr(attr: (String, String)): BtnLink = copy(attrs = attr :: attrs)

    def withHref(link: String) = copy(href = link)

    def tipTop(txt: String) = copy(cls = cls + " tip-top ", tipTop = txt).withExtraNs(<tail>{Script(Run( """$('.tip-top').tooltip({placement: 'top'});"""))}</tail>)
    def tipRight(txt: String) = copy(cls = cls + " tip-left ", tipTop = txt).withExtraNs(<tail>{Script(Run( """$('.tip-left').tooltip({placement: 'left'});"""))}</tail>)
    def tipLeft(txt: String) = copy(cls = cls + " tip-right ", tipTop = txt).withExtraNs(<tail>{Script(Run( """$('.tip-right').tooltip({placement: 'right'});"""))}</tail>)
    def tipBottom(txt: String) = copy(cls = cls + " tip-bottom ", tipTop = txt).withExtraNs(<tail>{Script(Run( """$('.tip-bottom').tooltip({placement: 'bottom'});"""))}</tail>)

    def apply(ns: NodeSeq) = this.ns

    def rendered = this.ns
  }

  object Btn {
    def Default = new BtnClas(cls = " btn ")
    def Primary = new BtnClas(cls = " btn btn-primary ")
    def Info = new BtnClas(cls = " btn btn-info ")
    def Success = new BtnClas(cls = " btn btn-success ")
    def Warning = new BtnClas(cls = " btn btn-warning ")
    def Danger = new BtnClas(cls = " btn btn-danger ")
    def Inverse = new BtnClas(cls = " btn btn-inverse ")
    def Extend = new BtnClas(cls = " btn btn-extend ")
  }

  class BtnGroup(btns: BtnGroup => Seq[NodeSeq], icn: Icn, name: => String, cls: String, style: String) {
    val id: String = Helpers.nextFuncName

    def withClass(cls: String) = new BtnGroup(btns, icn, name, this.cls + " " + cls, style)
    def withStyle(style: String) = new BtnGroup(btns, icn, cls, name, this.style + ";" + style)
    def withIcn(icn: Icn) = new BtnGroup(btns, icn, name, cls, style)

    def rerender(): JsCmd = SetHtml(id, rendered)

    def self = this

    def this(btns: NodeSeq*)(icn: Icn, name: => String = "", cls: String = "", style: String = "") = {
      this((_: BtnGroup) => btns, icn, name, cls, style)
    }

    def rendered =
      <div id={id} class="widget-action dropdown" style={style}>
        <a href="#" class="noselect dropdown-toggle" data-toggle="dropdown"><i class={icn.cls}></i> <span>{name}</span> <b class="caret"></b></a>
        <ul class="dropdown-menu pull-right"> {btns(this).map(b => <li>{b}</li>).foldLeft[NodeSeq](NodeSeq.Empty)(_ ++ _)}</ul>
      </div>
  }

}