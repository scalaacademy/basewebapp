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

package com.github.david04.liftutils.props

import net.liftweb.common.Box
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.json.JsonAST.{JField, JObject, JString}
import net.liftweb.json._
import net.liftweb.util.{BasicTypesHelpers, FatLazy}

class Var[T](initial: => T, getter: () => Option[T], setter: Option[T] => Unit) extends FatLazy[T](null.asInstanceOf[T]) {
  private var value = getter()

  override def defined_? = synchronized(value != None)
  override def get: T = synchronized {value.getOrElse({value = Some(initial); get})}
  override def set(n: T): T = synchronized {value = Some(n); setter(Some(n)); n}
  def :=(n: T): JsCmd = { set(n); onSet(n) }
  def apply() = get
  override def setFrom(other: FatLazy[T]): Unit = ???
  override def reset = synchronized {value = None}
  override def calculated_? = synchronized {value.isDefined}

  def onSet(v: T): JsCmd = JsCmds.Noop

  def self = this

  class View[U](toT: U => T, toU: T => U, onSet: U => JsCmd = (_: U) => JsCmds.Noop) {
    private var uValue = toU(get)
    def apply() = uValue
    def update(u: U) = {
      uValue = u
      val t = toT(u)
      set(t)
      self.onSet(t) & onSet(u)
    }
    def :=(n: U): JsCmd = update(n)

    def onSet(cmd: U => JsCmd): View[U] = new View[U](toT, toU, u => onSet(u) & cmd(u))
  }

  def view[U](toT: U => T, toU: T => U) = new View[U](toT, toU)
}

trait Props {

  protected def prefix = ""

  def get(key: String): Option[String]
  def set(key: String, value: Option[String]): Unit
  def set(key: String, value: String): Unit = set(key, Some(value))

  def getInt(key: String): Option[Int] = get(key).flatMap(s => BasicTypesHelpers.tryo(s.toInt))
  def setInt(key: String, value: Option[Int]): Unit = set(key, value.map(_.toString))
  def setInt(key: String, value: Int): Unit = setInt(key, Some(value))

  def getLong(key: String): Option[Long] = get(key).flatMap(s => BasicTypesHelpers.tryo(s.toLong))
  def setLong(key: String, value: Option[Long]): Unit = set(key, value.map(_.toString))
  def setLong(key: String, value: Long): Unit = setLong(key, Some(value))

  def getBoolean(key: String): Option[Boolean] = get(key).flatMap(s => BasicTypesHelpers.tryo(s.toBoolean))
  def getBoolean(key: String, dflt: Boolean): Boolean = getBoolean(key).getOrElse(dflt)
  def setBoolean(key: String, value: Option[Boolean]): Unit = set(key, value.map(_.toString))
  def setBoolean(key: String, value: Boolean): Unit = setBoolean(key, Some(value))

  def strVar(name: String, initial: => String): Var[String] = new Var(initial, () => get(name), (value: Option[String]) => set(name, value))
  def intVar(name: String, initial: => Int): Var[Int] = new Var(initial, () => getInt(name), (value: Option[Int]) => setInt(name, value))
  def longVar(name: String, initial: => Long): Var[Long] = new Var(initial, () => getLong(name), (value: Option[Long]) => setLong(name, value))
  def longOptVar(name: String, initial: => Option[Long] = None): Var[Option[Long]] = new Var(initial, () => getLong(name).map(Some(_)), (value: Option[Option[Long]]) => setLong(name, value.flatten))
  def boolVar(name: String, initial: => Boolean): Var[Boolean] = new Var(initial, () => getBoolean(name), (value: Option[Boolean]) => setBoolean(name, value))
  def listVarKeyStr[T](name: String, initial: => List[T], toKey: T => String, fromKey: String => T): Var[List[T]] =
    new Var(
      initial,
      () => get(name).map(s => s.split(",").toList.filter(_ != "").map(fromKey)),
      (value: Option[List[T]]) => set(name, value.map(_.map(toKey).mkString(",")))
    )
  def listVarKeyLong[T](name: String, initial: => List[T], toKey: T => Long, fromKey: Long => T): Var[List[T]] = listVarKeyStr(name, initial, v => toKey(v).toString, s => fromKey(s.toLong))
  def listVarKeyInt[T](name: String, initial: => List[T], toKey: T => Int, fromKey: Int => T): Var[List[T]] = listVarKeyStr(name, initial, v => toKey(v).toString, s => fromKey(s.toInt))

  def in(prefix: String) = {
    val _prefix = prefix
    val parent = this
    new Props {
      def get(key: String): Option[String] = parent.get(_prefix + "." + key)
      def set(key: String, value: Option[String]): Unit = parent.set(_prefix + "." + key, value)
    }
  }
}

trait JSonProps extends Props {

  implicit val formats = DefaultFormats

  protected def getJSon: String
  protected def setJSon(value: String): Unit

  def get(key: String): Option[String] = json.get.obj.collectFirst({ case JField(`key`, JString(s)) => s})
  def set(key: String, value: Option[String]): Unit = {
    json set JObject(value.map(v => JField(key, JString(v)) :: Nil).getOrElse(Nil) ::: json.get.obj.filter(_.name != key))
    setJSon(compactRender(json.get))
  }

  protected val json: FatLazy[JObject] = FatLazy({
    Box.asA[JObject](parse(getJSon)).getOrElse(JObject(Nil))
  })
}
