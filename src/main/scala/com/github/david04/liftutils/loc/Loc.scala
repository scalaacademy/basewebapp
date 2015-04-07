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

package com.github.david04.liftutils.loc

import java.io.{BufferedWriter, File, FileWriter}

import net.liftweb.http.S

import scala.xml._

object Loc {
  private val _missing = collection.mutable.HashSet[String]()

  def missing(loc: String) = {
//    println("Missing: " + loc)
    _missing += loc
    val fromFile = if (new File("missing.properties").exists()) {
      val s = scala.io.Source.fromFile("missing.properties")
      val lines = s.getLines().toList
      s.close()
      lines
    } else Seq()

    val bw = new BufferedWriter(new FileWriter("missing.properties"))
    bw.write((fromFile ++ _missing.toSeq).distinct.sorted.mkString("\n"))
    bw.close()
  }

  var processMissingLoc: Seq[String] => Unit = {
    missing => println("Missing i18n keys:\n" + missing.sorted.mkString("\n"))
  }

  val hook = new Thread() {
    override def run() {
      if (_missing.nonEmpty) processMissingLoc(_missing.toSeq.sorted)
    }
  }
  Runtime.getRuntime.addShutdownHook(hook)
}

trait LocP extends Loc {
  implicit def parent = this

  def parentLoc: Loc = null

  def locPrefix: String =
    Stream.iterate[Class[_]](this.getClass)(_.getSuperclass).dropWhile(_.getSimpleName.filter(_.isLetter).toLowerCase.startsWith("anon")).head
      .getSimpleName.filter(_.isLetter).splitAt(1) match {
      case (a, b) => a.toLowerCase + b
    }
}

trait LocC extends Loc {

  protected def p: Loc
  protected def n: String
  def parentLoc: Loc = p
  def locPrefix: String = n
}

trait Loc {

  def parentLoc: Loc

  def withPrefix(prefix: String) = {
    val t = this
    new Loc {
      override def parentLoc = t

      override def locPrefix: String = prefix
    }
  }

  def locPrefix: String

  def fullPrefix: List[String] =
    Option(parentLoc).map(_.fullPrefix.map(_ + "-")).getOrElse("" :: Nil).flatMap(p => List(p + locPrefix, p + "*"))


  def locParamOpt(suffix: String, params: (String, String)*): Option[String] = {
    if (suffix.startsWith("{")) {
      import net.liftweb.json._
      implicit val formats = DefaultFormats
      parse(suffix) match {
        case JObject(JField("suffix", JString(suffix)) :: JField("prefix", JString(prefix)) :: JField("params", JObject(params)) :: Nil) =>
          S.loc(prefix + "-" + suffix).map(_ => S.?(prefix + "-" + suffix)).map(loc =>
            params.foldLeft(loc)((str, param) => {
              val value = param.value.asInstanceOf[JString].s
              str.replaceAllLiterally("{" + param.name + "}", if (value.startsWith("{")) locProc(value) else value)
            }))
        case _ => ???
      }
    } else {
      fullPrefix.view.map(prefix => S.loc(prefix + "-" + suffix).map(_ => S.?(prefix + "-" + suffix))).flatten.headOption match {
        case Some(loc) => Some(params.foldLeft(loc)((str, param) => str.replaceAllLiterally("{" + param._1 + "}", param._2)))
        case None =>
          Loc.missing(fullPrefix.head + "-" + suffix + "=" + suffix)
          None
      }
    }
  }

  def locParam(suffix: String, params: (String, String)*): String =
    locParamOpt(suffix, params: _*).getOrElse(fullPrefix.head + "-" + suffix)

  def loc(suffix: String, param: (String, String), rest: (String, String)*): String = locParam(suffix, (param +: rest): _*)

  def locOpt(suffix: String): Option[String] = locParamOpt(suffix)

  def loc(suffix: String) = locParam(suffix)


  def locProc(value: String) = loc(value)

  def locUnproc(suffix: String, params: (String, String)*): String = {
    import net.liftweb.json.JsonDSL._
    import net.liftweb.json._
    val json =
      ("suffix" -> suffix) ~
        ("prefix" -> fullPrefix.head) ~
        ("params" -> JObject(params.toList.map(p => JField(p._1, JString(p._2)))))
    compactRender(json)
  }

  def processLoc(): NodeSeq => NodeSeq = {
    def runNodes(in: NodeSeq): NodeSeq = in.flatMap {
      case Group(g) => runNodes(g)
      case e: Elem => {
        e.attributes.collectFirst({ case UnprefixedAttribute("loc", key, _) => key.toString}) match {
          case Some(key) => new Elem(e.prefix, e.label,
            e.attributes.filter({ case UnprefixedAttribute("loc", key, _) => false case _ => true}),
            e.scope, Text(loc(key)))
          case None =>
            if (e.label == "loc") Text(e.child.headOption.map(_.toString).map(loc(_)).getOrElse(""))
            else new Elem(e.prefix, e.label, e.attributes, e.scope, e.child.map(runNodes(_).head): _*)
        }
      }
      case x => x
    }
    runNodes(_)
  }
}

trait LocEnum extends Enumeration {
  private def self = this

  private val locPrefix = this.getClass.getSimpleName.filter(_.isLetter).splitAt(1) match {
    case (a, b) => a.toLowerCase + b
  }
  private val _loc = new LocP {
    override def locPrefix: String = self.locPrefix
  }

  implicit def toLocValue(v: Value) = new {def loc: String = self._loc.loc(v.toString)}
}