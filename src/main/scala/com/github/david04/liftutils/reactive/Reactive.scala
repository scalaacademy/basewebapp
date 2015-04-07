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

package com.github.david04.liftutils.reactive

import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.{Replace, Run, _}
import net.liftweb.http.js.{JsCmd, JsCmds, JsExp}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference
import scala.xml.{Elem, NodeSeq}

// ======== Server side ========

trait SxVal[T] {

  protected var dependents: ListBuffer[WeakReference[SxDependent]] = ListBuffer()
  def addRxDependent(d: SxDependent): JsCmd = { dependents += WeakReference(d); Noop }
  protected def sxNotifyDependents(): JsCmd = dependents.map(_.underlying.get()).filter(_ != null).map(_.rxChanged()).foldLeft(JsCmds.Noop)(_ & _)

  def getRx: T
}

trait SxDependent {

  def rxChanged(): JsCmd
}

class SxFunc[D1, T](d1: SxVal[D1], f: D1 => T) extends SxVal[T] with SxDependent {

  d1.addRxDependent(this)

  def rxChanged(): JsCmd = sxNotifyDependents()

  def getRx: T = f(d1.getRx)
}

trait SxVar[T] extends SxVal[T] {
  protected val initialRx: T
  protected def extSet: Option[T => JsCmd] = None

  protected var current: Option[T] = None

  def setRx(v: T): JsCmd = {
    current = Some(v)
    extSet.map(_(v)).getOrElse(JsCmds.Noop) & sxNotifyDependents()
  }

  def getRx: T = {
    if (current.isEmpty) current = Some(initialRx)
    current.get
  }

  def apply() = getRx
  def update(v: T) = setRx(v)
}

object SxVar {
  def apply[T](_initial: T, _extSet: T => JsCmd) = new SxVar[T] {
    protected lazy val initialRx = _initial
    override protected val extSet: Option[T => JsCmd] = Some(_extSet)
  }
}

object RxRender {

  def apply[T](v: SxVal[T])(f: T => NodeSeq => NodeSeq): NodeSeq => NodeSeq = (ns: NodeSeq) => {

    val elem: Elem = ns.find {
      case e: Elem => true
      case _ => false
    }.map(_.asInstanceOf[Elem]).getOrElse(<span id={Helpers.nextFuncName}>{ns}</span>)

    val (withId: Elem, id: String) = Helpers.findOrAddId(elem)

    v.addRxDependent(new SxDependent {
      def rxChanged() = Replace(id, f(v.getRx)(withId))
    })

    f(v.getRx)(withId)
  }
}

// ======== Client side ========

trait JxVal {

  val id = Helpers.nextFuncName

  def jxAddDependent(d: JxDependent): JsCmd = {
    Run {
      s"{" +
        s"  var cur = window.change$id || (function() {});" +
        s"  window.change$id = function() {cur(); ${d.jxChanged().toJsCmd}};" +
        s"};"
    }
  }

  protected def jxNotifyDependents(): JsCmd = Run(s"if(window.change$id) {window.change$id();}")

  def getJx: JsExp
}

trait JxDependent {

  def jxChanged(): JsCmd
}

trait JxVar extends JxVal {

  protected val jxInitial: JsExp

  def getJx: JsExp = JsRaw(s"(window.V$id ? window.V$id : ${jxInitial.toJsCmd})")

  def setJx(v: JsExp) = Run(s"window.V$id = " + v.toJsCmd) & jxNotifyDependents()

  def initRX() = setJx(jxInitial)
  def initRXScript() = <tail>{Script(setJx(jxInitial))}</tail>
}

class JxFunc(d1: JxVal, f: JsExp => JsExp) extends JxVal {

  override def jxAddDependent(d: JxDependent): JsCmd = d1.jxAddDependent(d)

  override protected def jxNotifyDependents() = ???

  def getJx: JsExp = f(d1.getJx)
}

object JxVar {
  def apply[X](initial: JsExp) = new JxVar {
    protected lazy val jxInitial = initial
  }
}

// ======== Server & Client side ========

trait SJxVal[T] extends SxVal[T] with JxVal {

  /**
   * To be called on the server side.
   */
  def sxAddDependent(d: SJxDependent): JsCmd = addRxDependent(d) & jxAddDependent(d)

  /**
   * To be called on the client side.
   */
  def jxAddDependent(d: SJxDependent): JsCmd = jxAddDependent(d) & SHtml.ajaxInvoke(() => addRxDependent(d))

  /**
   * To be called on the server side.
   */
  override protected def sxNotifyDependents() = super.sxNotifyDependents() & jxNotifyDependents()

  /**
   * To be called on the client side.
   */
  override protected def jxNotifyDependents() = super.jxNotifyDependents() & SHtml.ajaxInvoke(() => sxNotifyDependents())
}

trait SJxDependent extends SxDependent with JxDependent {}

trait SJxVar[T] extends SxVar[T] with JxVar {

  protected val toJx: T => JsExp
  protected val fromJx: JValue => T

  protected lazy val jxInitial: JsExp = toJx(initialRx)

  def setSx(v: T): JsCmd = setRx(v) & setJx(toJx(v))

  def setJx(v: JsExp, afterSetClientSide: JsCmd = Noop, afterSetServerSide: () => JsCmd = () => Noop): JsCmd =
    setJx(v) &
      afterSetClientSide &
      SHtml.jsonCall(v, (v: JValue) => super.setRx(fromJx(v)) & afterSetServerSide())
}

object SJxVar {
  def apply[T](
                initial: T,
                _toRX: T => JsExp,
                _fromRX: JValue => T,
                _extSet: Option[T => JsCmd] = None) = new SJxVar[T] {
    protected val initialRx = initial
    protected val toJx = _toRX
    protected val fromJx = _fromRX
    override protected val extSet = _extSet
  }
}

trait JxStr extends JxVal

object RichJxStr {

  implicit class JxRichStr(s: JxStr) {
    def jxRender(): NodeSeq = {
      val id = Helpers.nextFuncName
      <tail>{Script(OnLoad {
        s.jxAddDependent(new JxDependent {
          override def jxChanged(): JsCmd = Run(s"${'$'}('#$id').text(" + s.getJx.toJsCmd + ");")
        }) & Run(s"${'$'}('#$id').text(" + s.getJx.toJsCmd + ");")
      })}</tail> ++
        <span id={id}></span>
    }

    def transform(f: String => String) = new JxFunc(s, jsExp => JsRaw(f(jsExp.toJsCmd))) with JxStr
    def maxLength(len: Int, ellipsis: String) = transform(s => s"(($s.length < $len) ? ($s) : ($s.substring(0,$len)+${ellipsis.encJs})+'')")
    def ifEmpty(dflt: String) = transform(s => s"((($s.length == 0) ? ${dflt.encJs} : $s)+'')")
  }

}


