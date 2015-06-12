package code.pages.theme

import java.util.UUID
import java.util.regex.Pattern

import com.github.david04.liftutils.reactive3.EmmiterStateful
import net.liftweb.common.Full
import net.liftweb.http.SHtml.{BasicElemAttr, ElemAttr, PairStringPromoter}
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.{JE, JsCmd, JsExp}
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq


trait BWAInPlaceEditing {

  object InPlace {

    class InPlace[T](
                      get: => T,
                      toStr: T => String,
                      set: T => JsCmd,
                      fromStr: String => Option[T],
                      ifEmpty: String = "",
                      setParentWidth: Boolean = true,
                      displayClasses: T => String = (_: T) => ""
                      ) extends EmmiterStateful {

      override def initialValue(): JsExp = JE.Str(toStr(get))

      var editing = false

      val id = UUID.randomUUID().toString

      lazy val renderer = SHtml.idMemoize(renderer => (_: NodeSeq) => {
        if (editing) {
          val cur = toStr(get)

          val setText = SHtml.ajaxCall(JsRaw(s"""$$('#$id').val()"""), s => {
            editing = false
            fromStr(s).map(set(_)).getOrElse(Noop) & renderer.setHtml() & setValue(JE.Str(toStr(get)))
          })

          <input id={id} type="text" onblur={(setValue(JsRaw(s"""$$('#$id').val()""")) & setText).toJsCmd} onkeyup={s"${setValue(JsRaw(s"""$$('#$id').val()""")).toJsCmd}"} onkeypress={s"liftUtils.lift_blurIfReturn(event)"} style={s"width:${math.max(80, cur.length * 5)}px"} class="inline-editor" value={cur}>{}</input>
        } else {
          val value = get
          val editMode = SHtml.ajaxInvoke(() => {editing = true; renderer.setHtml() & Run(s"""$$('#$id').focus().select();""")}).toJsCmd
          <div id={id} style="display:inline;" tabindex="0" class={"inplace-display " + displayClasses(value)} onclick={editMode}>{Some(toStr(value)).filter(_ != "").getOrElse(ifEmpty)}</div>
        }
      })

      def render = if (S.inStatefulScope_?) renderer.apply(<div style="display:inline;"></div>) else <span>{toStr(get)}</span>
    }

    class InPlaceSel[T](
                         get: => T,
                         toStr: T => String,
                         set: T => JsCmd,
                         all: Seq[T],
                         setParentWidth: Boolean = true,
                         attrs: Seq[ElemAttr] = Nil,
                         displayClasses: T => String = (_: T) => ""
                         ) extends EmmiterStateful {

      override def initialValue(): JsExp = JE.Str(toStr(get))

      var editing = false

      val id = UUID.randomUUID().toString

      lazy val renderer = SHtml.idMemoize(renderer => (_: NodeSeq) => {
        if (editing) {
          SHtml.ajaxSelectElem[T](
            all,
            Full(get),
            (new BasicElemAttr("class", "inline-editor") +: attrs): _*
          )(v => {
            editing = false
            set(v) & renderer.setHtml() & setValue(JE.Str(toStr(get)))
          })(new PairStringPromoter[T] {
            override def apply(v1: T): String = toStr(v1)
          })

        } else {
          val value = get
          val editMode = SHtml.ajaxInvoke(() => {editing = true; renderer.setHtml() & Run(s"""$$('#$id').focus().select();""")}).toJsCmd
          <div id={id} style="display:inline;" tabindex="0" class={"inplace-display " + displayClasses(value)} onclick={editMode}>{toStr(value)}</div>
        }
      })

      def render = if (S.inStatefulScope_?) renderer.apply(<div style="display:inline;"></div>) else <span>{toStr(get)}</span>
    }

    def str(get: => String, set: String => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false) =
      new InPlace[String](get, s => s, set, s => Some(s), ifEmpty, setParentWidth)

    def sel[T](get: => T, set: T => JsCmd, all: Seq[T], toStr: T => String = (_: T).toString, setParentWidth: Boolean = false, attrs: Seq[ElemAttr] = Nil) =
      new InPlaceSel(get, toStr, set, all, setParentWidth, attrs)

    def pw(set: String => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, serverVal: Boolean = true) =
      new InPlace[String]("", s => s, set, s => Some(s), ifEmpty, setParentWidth)

    def double(get: => Double, set: Double => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, append: String = "", fmt: String = "%.2f") =
      new InPlace[Double](get, _.formatted(fmt) + append, set, s => tryo(s.replaceAll(Pattern.quote(append) + "$", "").toDouble).toOption, ifEmpty, setParentWidth)

    def doubleOpt(get: => Option[Double], set: Option[Double] => JsCmd, ifEmpty: String = "", setParentWidth: Boolean = false, append: String = "", fmt: String = "%.2f") =
      new InPlace[Option[Double]](
        get,
        _.map(_.formatted(fmt) + append).getOrElse(""),
        set,
        s => tryo(if (s == "") None else Some(s.replaceAll(Pattern.quote(append) + "$", "").toDouble)).toOption,
        ifEmpty,
        setParentWidth,
        displayClasses = _.map((_: Double) => "").getOrElse("empty-value")
      )
  }

}
