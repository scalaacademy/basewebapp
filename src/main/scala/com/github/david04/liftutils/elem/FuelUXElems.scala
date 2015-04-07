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

package com.github.david04.liftutils.elem

import net.liftweb.common.{Full, _}
import net.liftweb.http._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{Run, _}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.util.Helpers._

import scala.xml._

object FuelUXTreeValidation {

  trait ReqFuelUXTree extends FuelUXTree {
    override def error() =
      (if (getCurrentValue() == None) Some(Text(labelStr("errorReq"))) else None) orElse super.error()
  }

}

case class DataAttributes(id: String = S.formFuncName)

case class FuelUXNode(id: String, name: String, `type`: String, isSelected: Boolean, dataAttributes: DataAttributes = DataAttributes())(implicit val tree: FuelUXTree) {
  def selectNode() = Run("$('#" + tree.id('tree) + "').tree('selectItem', $('#" + dataAttributes.id + "'));")
}

trait FuelUXTree extends HTMLEditableElem with LabeledElem {
  implicit def self = this

  protected val allowSelectStar: Boolean

  override protected def htmlElemTemplatePath: List[String] = "templates-hidden" :: "elem-edit-tree-dflt" :: Nil

  type ID = String
  type Type = String

  def all: Seq[String]

  def get: () => Option[String]

  def set: Option[String] => JsCmd

  val initialValue = get()
  var inited = false
  var selected: Option[Node] = None

  def save(): JsCmd = set(getCurrentValue())

  def getCurrentValue(): Option[String] = if (inited) selected.map(_.value) else initialValue

  case class Node(id: ID, value: String, name: String, `type`: Type, children: () => Array[Node])

  private def lazyF[T](f: => T) = { lazy val v = f; () => v }

  def toFuelUX(node: Node) = new FuelUXNode(node.id, node.name, node.`type`, getCurrentValue() == Some(node.value))

  override protected def htmlElemRendererTransforms = {

    implicit val formats = net.liftweb.json.DefaultFormats

    val map = collection.mutable.Map[ID, Node]()

    def recur(pre: String, all: Seq[Array[String]]): Array[Node] =
      (if (allowSelectStar) Array("*") +: all else all)
        .groupBy(_.head).mapValues(children => {
        val tail = children.map(_.tail)
        (tail.filterNot(_.isEmpty), tail.exists(_.isEmpty))
      })
        .toArray
        .sortWith((s1, s2) => {
        if (s1._1.startsWith("*")) false
        else if (s2._1.startsWith("*")) true
        else s1._1.compareTo(s2._1) < 0
      })
        .flatMap({
        case (cur, (children, includeSelf)) =>
          lazy val selfNode = {
            val node = Node(S.formFuncName, pre + cur, cur, "item", () => Array())
            map(node.id) = node
            node
          }
          lazy val childrenNode = {
            val node = Node(S.formFuncName, pre, cur + ".", "folder", lazyF[Array[Node]](recur(s"$pre$cur.", children)))
            map(node.id) = node
            node
          }

          if (children.isEmpty) Array(selfNode)
          else if (!children.isEmpty && includeSelf) Array(selfNode, childrenNode)
          else Array(childrenNode)
      })

    val root = recur("", all.map(_.split("\\.")))

    val currentSelectionRenderer = SHtml.idMemoize(_ => (_: NodeSeq) =>
      Text(labelStr("current").replaceAllLiterally("{value}", getCurrentValue().getOrElse(labelStr("none")))))

    val script =
      Script(OnLoad(Run(
        "$('#" + id('tree) + "').tree({dataSource: { data: function(opt, cb) { " +
          SHtml.jsonCall(JsRaw("opt"),
            new JsonContext(Full("function(v){cb({data: v.nodes}); eval(v.init);}"), Empty),
            (v: JValue) => {

              val nodes = (for {
                obj <- Box.asA[JObject](v)
                idValue <- obj.values.get("id")
                id <- Box.asA[String](idValue)
              } yield {
                map(id).children().map(toFuelUX(_))
              }) openOr root.map(toFuelUX(_))

              ((("nodes", Extraction.decompose(nodes))) ~
                (("init", nodes.filter(_.isSelected).map(_.selectNode()).foldLeft[JsCmd](Noop)(_ & _).toJsCmd.toString)))
            }
          ).toJsCmd +
          "}}}).on('updated', function(sel) {" +
          SHtml.jsonCall(JsRaw("$('#" + id('tree) + "').tree('selectedItems').map(function(i) {return i.id;})"),
            (v: JValue) => v match {
              case JArray((sel: JString) :: rest) =>
                inited = true
                selected = Some(map(sel.s))
                currentSelectionRenderer.setHtml() & onChangeClientSide()
              case _ =>
                inited = true
                selected = None
                currentSelectionRenderer.setHtml() & onChangeClientSide()
            }).toJsCmd +
          "});")))

    super.htmlElemRendererTransforms andThen
      (".elem-wrap [style+]" #> (if (!enabled()) "display:none;" else "") &
        ".elem-wrap [id]" #> id('wrapper) &
        ".elem-lbl *" #> wrapName(labelStr) &
        ".elem-error [id]" #> id('error) &
        ".tree-title-lbl" #> currentSelectionRenderer) andThen
      (".tree [id]" #> id('tree)) andThen
      ((ns: NodeSeq) => ns ++ <tail>
        {script}
      </tail>)
  }
}

trait FuelUXModalEditTree extends FuelUXTree with ModalEditElem {
  override protected def htmlModalEditableElemViewTemplatePath: List[String] =
    "templates-hidden" :: "elem-modaledit-tree-view-dflt" :: Nil

  protected def getCurrentViewString(): String = getCurrentValue().getOrElse(labelStr("none"))

  override def onChangeClientSide(): JsCmd =
    super.onChangeClientSide() & setCurrentViewString(getCurrentValue().getOrElse(labelStr("none")))

}