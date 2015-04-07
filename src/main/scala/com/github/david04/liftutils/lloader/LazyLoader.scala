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

package com.github.david04.liftutils.lloader

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

import net.liftweb.common.Full
import net.liftweb.http.js.JsCmds.{Run, _}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.{IdMemoizeTransform, NodeSeqFuncOrSeqNodeSeqFunc, S, SHtml}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import scala.xml.{Elem, NodeSeq}
import scala.xml.NodeSeq._

/**
 * Lazy Loader
 */

object LazyLoader {
  val defaultInterval = 1000
  val defaultPoolSize = 6
  def createDefaultPool(): ExecutorService = Executors.newFixedThreadPool(defaultPoolSize)
}

object DfltLL extends LazyLoader(<div></div>) {

  override def blockUI(id: String): JsCmd = Noop
  override def unblockUI(id: String): JsCmd = Noop
}

class LazyLoader(
                  defaultLoadingTemplate: => NodeSeq,
                  interval: Int = LazyLoader.defaultInterval,
                  val pool: ExecutorService = LazyLoader.createDefaultPool()
                  ) {

  def blockUI(id: String): JsCmd = Run("$('#" + id + "').block({ message: null });")
  def unblockUI(id: String): JsCmd = Run("$('#" + id + "').unblock();")

  val left = collection.mutable.ListBuffer[Future[JsCmd]]()

  def callback(): JsCmd = {
    val variable = S.formFuncName
    val running = "window." + S.formFuncName
    Run(
      s"window.$variable = window.setInterval(function() {" +
        s"if(!$running) {" +
        s"  $running = true;" +
        SHtml.ajaxInvoke(() => {
          val finished =
            left.synchronized({
              val toRemove = left.filter(f => f.isDone || f.isCancelled).toList
              left --= toRemove
              toRemove
            })
          val updates = finished.filter(_.isDone).flatMap(f => tryo(f.get())).foldLeft(JsCmds.Noop)(_ & _)
          Run(s"$running = false;") & updates
          //& Run(if (left.isEmpty) s";window.clearTimeout(window.$variable);" else "")
        }).toJsCmd +
        "  }" +
        "}" +
        s",$interval);")
  }

  def loaderScript(): JsCmd = callback()
  def loader(): NodeSeq = <tail>{Script(OnLoad(callback()))}</tail>
  def installLoader(): NodeSeq => NodeSeq = (ns: NodeSeq) => ns ++ loader()

  def idMemoize(f: IdMemoizeTransform => NodeSeqFuncOrSeqNodeSeqFunc, loadingTemplate: NodeSeq = defaultLoadingTemplate): IdMemoizeTransform = {
    new IdMemoizeTransform {
      val self = this
      var loadedOnce = false

      var latestElem: Elem = <span/>

      var latestKids: NodeSeq = NodeSeq.Empty

      var latestId = Helpers.nextFuncName

      private def fixElem(e: Elem): Elem = {
        e.attribute("id") match {
          case Some(id) => latestId = id.text; e
          case None => e % ("id" -> latestId)
        }
      }

      def apply(ns: NodeSeq): NodeSeq =
        Helpers.findBox(ns) {
          e => latestElem = fixElem(e);
            latestKids = e.child;
            Full(e)
        }.
          map(ignore => applyAgain()).openOr(NodeSeq.Empty)

      def load() = {
        left.synchronized(left += pool.submit(new Callable[JsCmd] {def call(): JsCmd = try {SetHtml(latestId, f(self)(latestKids))} finally {loadedOnce = true}}))

        <div id={latestId}>{loadingTemplate}</div>
      }

      def applyAgain(): NodeSeq =
        new Elem(latestElem.prefix,
          latestElem.label,
          latestElem.attributes,
          latestElem.scope,
          load(): _*)

      def setHtml(): JsCmd = {
        left.synchronized(left += pool.submit(new Callable[JsCmd] {
          def call(): JsCmd = SetHtml(latestId, f(self)(latestKids)) & unblockUI(latestId)
        }))
        if (loadedOnce) blockUI(latestId) else Noop
      }
    }
  }

}
