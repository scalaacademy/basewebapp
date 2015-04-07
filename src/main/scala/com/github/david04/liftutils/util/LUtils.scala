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

package com.github.david04.liftutils.util

import java.text.SimpleDateFormat

import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsNull
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.json.JsonAST.JValue
import net.liftweb.util.Helpers._

import scala.xml._


object LUtils {

  var enableProfiling = true

  def iefix = Unparsed( """
                          | <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
                          | <!--[if lt IE 9]>
                          | <script src="/js/html5shiv.js"></script>
                          | <![endif]-->
                          |
                          | """.stripMargin)

  implicit class MAp[T](v: T) {
    def $$(f: T => Unit): T = { f(v); v }
    def -->[U](f: T => U): U = f(v)
  }

  implicit class ToOption[T](v: T) {
    def option: Option[T] = Option(v)
  }

  implicit def __print[T](v: T) = new {
    def #!(s: String = "", t: T => String = _.toString): T = { println(s + t(v)); v }

    def #! : T = #!("")
  }

  def run(b: => JsCmd) = SHtml.jsonCall(JsNull, (_: JValue) => b).toJsCmd + ";"

  def runJsCmd(b: => JsCmd) = SHtml.jsonCall(JsNull, (_: JValue) => b)

  def time[T](f: => T)(r: Long => Unit): T = {
    val start = System.currentTimeMillis()
    val v = f
    r(System.currentTimeMillis() - start)
    v
  }

  def printNs = (ns: NodeSeq) => {println(ns); ns}
  def printNs(s: String) = (ns: NodeSeq) => {println(s + ":\n" + ns); ns}

  var idx = new ThreadLocal[Int]() {override def initialValue(): Int = 0}
  var pending = new ThreadLocal[List[String]]() {override def initialValue(): List[String] = Nil}
  var minTime = new ThreadLocal[List[Long]]() {override def initialValue(): List[Long] = Nil}
  var lastOpen = false
  var enabled = new ThreadLocal[Boolean]() {override def initialValue(): Boolean = true}

  /**
   * Profile
   */
  def %?[T](s: String)(b: => T): T = %?[T](s, null, 0)(b)

  def %%?[T](s: String, minTime: Long = 1000)(b: => T): T = %?[T](s, null, minTime)(b)

  def %??[T1, T2](s: String)(f: T1 => T2): T1 => T2 = (v: T1) => %?(s)(f(v))

  def disableProfiling[T](b: => T): T = {
    enabled set false
    val (ret, ex) = try {
      val ret = b
      (Some(ret), None)
    } catch {
      case t: Throwable => (None, Some(t))
    }
    enabled set true
    (ret, ex) match {
      case (Some(ret), _) => ret
      case (_, Some(t)) => throw t
      case _ => ???
    }
  }

  val fmt = new SimpleDateFormat("dd/MM/yy HH:mm:ss:SSS")


  def %?[T](s: String, rslt: T => String, _minTime: Long)(b: => T): T = {
    if (enabled.get) {
      val space = (0 until idx.get).map(_ => "  ").mkString

      minTime set ((math.max(_minTime, minTime.get.headOption.getOrElse(0L))) :: minTime.get)

      val MIN_TIME: Long = minTime.get.head

      val init =
        (if (lastOpen) System.lineSeparator() else "") +
          (space + s"[${fmt.format(new java.util.Date())}@${Thread.currentThread().getName.takeRight(10)}] Starting '$s'...")

      if (MIN_TIME == 0) print(init)
      else pending set (init :: pending.get)

      lastOpen = true
      idx set (idx.get + 2)

      val start = System.currentTimeMillis()
      val (ret, ex) = try {
        val ret = b
        (Some(ret), None)
      } catch {
        case t: Throwable => (None, Some(t))
      }
      val took = System.currentTimeMillis() - start

      idx set (idx.get - 2)

      val exception = ex match {
        case Some(t) => {
          s" !! {Exception: '${t.getMessage}'} @ " + Thread.currentThread().getStackTrace.mkString("\n", "\n", "\n")
        }
        case None => ""
      }

      val rsltStr = ret.flatMap(ret => Option(rslt).map(_(ret))).map(" [" + _ + "]").getOrElse("")

      val closing = if (lastOpen && MIN_TIME == 0) s" [${took}ms]$exception$rsltStr" else s"${space}[${fmt.format(new java.util.Date())}@${Thread.currentThread().getName.takeRight(10)}] Finished '$s' [${took}ms]$exception$rsltStr"

      if (MIN_TIME == 0) println(closing)
      else if (took >= MIN_TIME) {
        println(pending.get.reverse.mkString("\n"))
        println(closing)
        pending set Nil
      } else {
        pending set (pending.get.tail)
      }

      minTime set minTime.get.tail

      ex.foreach(_.printStackTrace())
      lastOpen = false

      (ret, ex) match {
        case (Some(ret), _) => ret
        case (_, Some(t)) => throw t
        case _ => ???
      }
    } else {
      b
    }

  }

  implicit class RichJsCmd(c: JsCmd) {
    def P = Run("console.log(" + c.toJsCmd.encJs + ");") & c
  }


  case class Memo[A, B](f: A => B) extends (A => B) {
    val cache = collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate(x, f(x))

    def this(pf: PartialFunction[A, B]) = this((a: A) => pf(a))

    def prefetch(values: List[(A, B)]) = cache ++= values
  }

}
