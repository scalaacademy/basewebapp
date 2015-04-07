/*
        Copyright 2011 Spiral Arm Ltd

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.package bootstrap.liftmodules
*/
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

package com.github.david04.liftutils.jplot

import java.text.SimpleDateFormat

import net.liftweb.common._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers

import scala.xml.{Comment, NodeSeq}

trait JQPlotLibrary {

  def pluginsBase: String

  def jsBase: String

  def cssBase: String

  def resizable = true

  class JqPlot(w: String, h: String, options: Option[Options], series: List[List[Any]]*) extends Loggable {

    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm")

    val id = Helpers.nextFuncName

    val plotVar = "jqplot" + id

    val style = "height:%s; width:%s;".format(h, w)

    val plugins = options match {
      case Some(op) =>
        for {plugin <- op.plugins} yield
          <script type="text/javascript" src={"%sjqplot.%s.min.js".format(pluginsBase, plugin.name)}></script>
      case otherwise => Nil
    }

    def toCssTransformer: NodeSeq => NodeSeq = { _ => toHtml }

    def toHtml = {
      val jsonSeries = series.map {
        s => JArray(s.map {
          v => JArray(
            v.map {
              case x: Int => JInt(x)
              case x: Long => JInt(x)
              case x: Double => JDouble(x)
              case x: String => JString(x)
              case x: java.util.Date => JString(sdf.format(x))
              case x =>
                logger.error("Unknown type %s".format(x))
                JNothing(42)
            }
          )
        })
      }.toList
      val hack = JsRaw( """function _hackJsonFunctions(json) {
               //https://bitbucket.org/cleonello/jqplot/issue/139/allows-basic-json-to-work-with-jqplot
	            for (var k in json) {
	            if (typeof json[k] == 'function') {
	                continue;
	            }
	            if (typeof json[k] == 'object') {
	                json[k] = _hackJsonFunctions(json[k]);
	            }
	            if (k == 'renderer') {
	                json[k] = eval(json[k]);
	            }
	            }
	        	return json;
            }""")

      val onLoadJs = {

        val series = "series_%s".format(id)
        val opt = "options_%s".format(id)

        val y = new JsCrVar(series, JArray(jsonSeries))
        val z = Run("document.%s = $.jqplot('%s',%s, _hackJsonFunctions(%s));".format(plotVar, id, series, opt))

        options match {
          case Some(o) => OnLoad(new JsCrVar(opt, o.toJson) & hack & y & z)
          case otherwise => OnLoad(new JsCrVar(opt, "") & hack & y & z)
        }
      }

      val ie = Comment(
        """[if lt IE 9]>
          | <script language="javascript" type="text/javascript" src="%sexcanvas.min.js"></script>
          | <![endif]""".format(jsBase))

      val resize = if (resizable)
        Script(OnLoad(Run(
          """$(window).resize(function() { document.%s.replot( { resetAxes: true } ); });""".format(plotVar))))
      else NodeSeq.Empty


      <span>
        <head_merge>
          <link rel="stylesheet" type="text/css" href={cssBase + "jquery.jqplot.min.css"} />
        </head_merge>
        <tail>
          {ie}
          <script type="text/javascript" src={ jsBase + "jquery.jqplot.min.js" }></script>
          { plugins }
          { Script(onLoadJs) }
          { resize }
        </tail>
        <div id={ id } style={ style }></div>
      </span>

   }
  }

  trait PlotSnippet {

    implicit def pair2ListAny(p: Seq[(Any, Any)]): List[List[Any]] = p.map {x => List(x._1, x._2)} toList

    implicit def pair2ListAny(p: (Any, Any)): List[Any] = List(p._1, p._2)

    implicit def series2Array(s: Series): Array[Series] = Array(s)

    implicit def series2Array(o: Options): Box[Options] = Box !! o

    val w = "100%"
    val h = "100%"

    val options: Box[Options] = Empty

    val series: Array[List[List[Any]]]

    def render = new JqPlot(w, h, options, series: _*).toCssTransformer
  }

}

case class Options(title: Option[Title] = None,
                   axisDefault: Option[Axis] = None,
                   axes: Option[Axes] = None,
                   seriesColors: Option[SeriesColors] = None,
                   seriesDefault: Option[Series] = None,
                   series: Option[MultipleSeries] = None,
                   legend: Option[Legend] = None,
                   grid: Option[Grid] = None,
                   highLighter: Option[HighLighter] = None,
                   cursor: Option[Cursor] = None) {

  def title(t: String): Options = this.copy(title = Some(Title(t)))

  def seriesColors(c: SeriesColors): Options = this.copy(seriesColors = Some(c))

  def legend(l: Legend): Options = this.copy(legend = Some(l))

  def grid(g: Grid): Options = this.copy(grid = Some(g))

  def axisDefault(a: Axis): Options = this.copy(axisDefault = Some(a))

  def axes(a: Axes): Options = this.copy(axes = Some(a))

  def seriesDefault(s: Series): Options = this.copy(seriesDefault = Some(s))

  def series(s: List[Series]): Options = this.copy(series = Some(MultipleSeries(Some(s))))

  def highLighter(h: HighLighter): Options = this.copy(highLighter = Some(h))

  def cursor(c: Cursor): Options = this.copy(cursor = Some(c))

  private def fields = List(title, seriesColors, axisDefault, axes, legend, grid, seriesDefault, series, highLighter, cursor)

  private val renderers: List[Option[Renderable]] = List(axisDefault, axes, seriesDefault, series, grid, highLighter, cursor)

  def plugins = (for {b <- renderers; render <- b} yield render.renderers).flatten.distinct

  def toJson = JObject(for {b <- fields; t <- b} yield t.toJson)

}

trait JSONable extends Loggable {

  def toJson: JField

  protected def toJValue(x: Any): JValue = x match {
    case s: String => JString(s)
    case i: Int => JInt(i)
    case l: Long => JInt(l)
    case d: Double => JDouble(d)
    case l: Location => JString(l.toString())
    case b: Boolean => JBool(b)
    case r: Renderer => JString(r.toString())
    case m: MarkerOption => m.toJObject
    case m: MarkerStyle => JString(m.toString())
    case a: AxisName => JString(a.toString())
    case t: TickOptions => t.toJObject
    case h: HighLighter => h.toJObject
    case j: JSONable => j.toJson
    case otherwise => logger.error("We didn't cater for %s, sorry.".format(otherwise))
      JNull

  }

}

trait Renderable {

  val possible_renderers: List[Option[Any]]

  def renderers: List[Renderer] = {
    val r = for {b <- possible_renderers; render <- b} yield {
      if (render.isInstanceOf[Renderable]) {render.asInstanceOf[Renderable].renderers}
      else if (render.isInstanceOf[Renderer]) {List(render.asInstanceOf[Renderer]): List[Renderer]}
      else if (render.isInstanceOf[List[Any]]) {(for {i <- render.asInstanceOf[List[Any]] if i.isInstanceOf[Renderable]} yield {i.asInstanceOf[Renderable].renderers}).flatten}
      else Nil: List[Renderer]
    }
    r.flatten
  }
}

case class Title(name: String) extends JSONable {override def toJson = JField("title", JString(name))}

case class SeriesColors(seriesColors: List[String]) extends JSONable {

  def toJson = JField("seriesColors", JArray(seriesColors.map(JString(_))))
}

case class Axes(xaxis: Option[Axis] = None, yaxis: Option[Axis] = None, x2axis: Option[Axis] = None, y2axis: Option[Axis] = None) extends JSONable with Renderable {

  def xaxis(x: Axis): Axes = this.copy(xaxis = Some(x))

  def x2axis(x: Axis): Axes = this.copy(x2axis = Some(x))

  def yaxis(y: Axis): Axes = this.copy(yaxis = Some(y))

  def y2axis(y: Axis): Axes = this.copy(y2axis = Some(y))


  private def fields: List[(String, Option[Axis])] = List(("xaxis", xaxis),
    ("x2axis", x2axis),
    ("yaxis", yaxis),
    ("y2axis", y2axis))

  override def toJson = { JField("axes", JObject(for {b <- fields; t <- b._2} yield JField(b._1, t.toJObject))) }

  override val possible_renderers: List[Option[Any]] = List(xaxis, yaxis, x2axis, y2axis)

}

sealed trait Renderer {

  def name = {
    this.getClass.getSimpleName match {
      case n@"BezierCurveRenderer" => n
      case n@"OHLCRenderer" => "ohlcRenderer"
      case n@"HighLighterRenderer" => "highlighter"
      case n@"CursorRenderer" => "cursor"
      case n => n.replaceFirst(n.take(1), n.take(1).toLowerCase)
    }

  }

  override def toString = "$.jqplot.%s".format(this.getClass.getSimpleName)

}

case class AxisLabelRenderer() extends Renderer

case class BarRenderer() extends Renderer

case class BubbleRenderer() extends Renderer

case class HighLighterRenderer() extends Renderer

case class CursorRenderer() extends Renderer

case class CanvasAxisLabelRenderer() extends Renderer

case class CanvasAxisTickRenderer() extends Renderer

case class CategoryAxisRenderer() extends Renderer

case class DateAxisRenderer() extends Renderer

case class LinearAxisRenderer() extends Renderer

case class MeterGaugeRenderer() extends Renderer

case class OHLCRenderer() extends Renderer

case class PieRenderer() extends Renderer

case class TickOptions(formatString: Option[String] = None) extends JSONable {

  def formatString(s: String): TickOptions = this.copy(formatString = Some(s))

  private def fields: List[(String, Option[Any])] = List(("formatString", formatString))

  def toJObject = JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))

  def toJson = JField("tickOptions", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t))))

}

sealed trait AxisName {override def toString = this.getClass.getSimpleName}

sealed trait XAxisName extends AxisName

sealed trait YAxisName extends AxisName

case class xaxis() extends AxisName with XAxisName

case class x2axis() extends AxisName with XAxisName

case class yaxis() extends AxisName with YAxisName

case class y2axis() extends AxisName with YAxisName


//TODO: Add ticks
case class Axis(label: Option[String] = None,
                labelRenderer: Option[Renderer] = None,
                labelOptions: Option[LabelOptions] = None,
                min: Option[String] = None,
                max: Option[String] = None,
                pad: Option[String] = None,
                ticks: Option[List[Any]] = None,
                numberOfTicks: Option[Int] = None,
                renderer: Option[Renderer] = None,
                rendererOptions: Option[RenderOptions] = None,
                tickOptions: Option[TickOptions] = None,
                showTicks: Option[Boolean] = None,
                showTickMarks: Option[Boolean] = None,
                syncTicks_? : Option[Boolean] = None) extends JSONable with Renderable {

  def label(l: String): Axis = this.copy(label = Some(l))

  def labelRenderer(l: Renderer): Axis = this.copy(labelRenderer = Some(l))

  def labelOptions(l: LabelOptions): Axis = this.copy(labelOptions = Some(l))

  def min(m: String): Axis = this.copy(min = Some(m))

  def max(m: String): Axis = this.copy(max = Some(m))

  def pad(p: String) = this.copy(pad = Some(p))

  def ticks(l: List[Any]): Axis = this.copy(ticks = Option(l))

  def numberOfTicks(i: Int): Axis = this.copy(numberOfTicks = Some(i))

  def renderer(r: Renderer): Axis = this.copy(renderer = Some(r))

  def rendererOptions(r: RenderOptions): Axis = this.copy(rendererOptions = Some(r))

  def tickOptions(t: TickOptions): Axis = this.copy(tickOptions = Some(t))

  def showTicks(b: Boolean): Axis = this.copy(showTicks = Some(b))

  def showTickMarks(b: Boolean): Axis = this.copy(showTickMarks = Some(b))

  def syncTicks: Axis = this.copy(syncTicks_? = Some(true))

  private def fields: List[(String, Option[Any])] = List(("label", label),
    ("min", min),
    ("max", max),
    ("pad", pad),
    ("ticks", ticks),
    ("numberTicks", numberOfTicks),
    ("renderer", renderer),
    ("rendererOptions", rendererOptions),
    ("tickOptions", tickOptions),
    ("showTicks", showTicks),
    ("showTickMarks", showTickMarks),
    ("syncTicks", syncTicks_?))


  def toJObject = { JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t))) }

  def toJson = { JField("axesDefaults", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

  override val possible_renderers = List(renderer, labelRenderer)

}


sealed trait MarkerStyle {override def toString = this.getClass.getSimpleName.toLowerCase()}

case class circle() extends MarkerStyle

case class diamond() extends MarkerStyle

case class square() extends MarkerStyle

case class filledCircle() extends MarkerStyle

case class filledDiamond() extends MarkerStyle

case class filledSquare() extends MarkerStyle

case class custom(marker: String) extends MarkerStyle {override def toString = marker}

case class MarkerOption(show: Option[Boolean] = None,
                        style: Option[MarkerStyle] = None,
                        lineWidth: Option[Int] = None,
                        size: Option[Int] = None,
                        color: Option[String] = None,
                        showShadow: Option[Boolean] = None,
                        shadowAngle: Option[Int] = None,
                        shadowOffset: Option[Double] = None,
                        shadowDepth: Option[Int] = None,
                        shadowAlpha: Option[Double] = None) extends JSONable {

  def show(b: Boolean): MarkerOption = this.copy(show = Some(b))

  def style(ms: MarkerStyle): MarkerOption = this.copy(style = Some(ms))

  def lineWidth(w: Int): MarkerOption = this.copy(lineWidth = Some(w))

  def size(s: Int): MarkerOption = this.copy(size = Some(s))

  def color(c: String): MarkerOption = this.copy(color = Some(c))

  def showShadow(b: Boolean): MarkerOption = this.copy(showShadow = Some(b))

  def shadowAngle(i: Int): MarkerOption = this.copy(shadowAngle = Some(i))

  def shadowOffset(d: Double): MarkerOption = this.copy(shadowOffset = Some(d))

  def shadowDepth(i: Int): MarkerOption = this.copy(shadowDepth = Some(i))

  def shadowAlpha(d: Double): MarkerOption = this.copy(shadowAlpha = Some(d))


  private def fields: List[(String, Option[Any])] = List(("show", show),
    ("style", style),
    ("lineWidth", lineWidth),
    ("size", size),
    ("color", color),
    ("shadow", showShadow),
    ("shadowAngle", shadowAngle),
    ("shadowOffset", shadowOffset),
    ("shadowDepth", shadowDepth),
    ("shadowAlpha", shadowAlpha))

  def toJObject: JObject = { JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t))) }


  override def toJson = { JField("markerOptions", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }


}

case class MultipleSeries(ss: Option[List[Series]] = None) extends JSONable with Renderable {

  def series(l: List[Series]): MultipleSeries = this.copy(ss = Some(l))

  override val possible_renderers = List(ss)

  override def toJson = ss match {
    case Some(l) => JField("series", JArray(for {s <- l} yield s.toJObject))
    case None => JField("series", JArray(List()))
  }

}


case class RenderOptions(
                          varyBarColor: Option[Boolean] = None
                          ) extends JSONable {

  def fields: List[(String, Option[Any])] = List(
    ("varyBarColor", varyBarColor)
  )

  override def toJson = { JField("rendererOptions", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }


}

case class Series(xaxis: Option[XAxisName] = None,
                  yaxis: Option[YAxisName] = None,
                  label: Option[String] = None,
                  colour: Option[String] = None,
                  lineWidth: Option[Int] = None,
                  displayLine: Option[Boolean] = None,
                  showShadow: Option[Boolean] = None,
                  shadowAngle: Option[Int] = None,
                  shadowOffset: Option[Double] = None,
                  shadowWidth: Option[Int] = None,
                  shadowDepth: Option[Int] = None,
                  shadowAlpha: Option[Double] = None,
                  fill: Option[Boolean] = None,
                  fillAndStroke: Option[Boolean] = None,
                  fillColor: Option[String] = None,
                  fillAlpha: Option[Double] = None,
                  renderer: Option[Renderer] = None,
                  rendererOptions: Option[RenderOptions] = None,
                  markerOptions: Option[MarkerOption] = None,
                  color: Option[String] = None) extends JSONable with Renderable {

  def xaxis(axis: XAxisName): Series = this.copy(xaxis = Option(axis))

  def yaxis(axis: YAxisName): Series = this.copy(yaxis = Option(axis))

  def label(l: String): Series = this.copy(label = Some(l))

  def colour(c: String): Series = this.copy(colour = Some(c))

  def lineWidth(w: Int): Series = this.copy(lineWidth = Some(w))

  def showLine: Series = this.copy(displayLine = Some(true))

  def hideLine: Series = this.copy(displayLine = Some(false))

  def showShadow(b: Boolean): Series = this.copy(showShadow = Some(b))

  def shadowAngle(i: Int): Series = this.copy(shadowAngle = Some(i))

  def shadowOffset(d: Double): Series = this.copy(shadowOffset = Some(d))

  def shadowWidth(i: Int): Series = this.copy(shadowWidth = Some(i))

  def shadowDepth(i: Int): Series = this.copy(shadowDepth = Some(i))

  def shadowAlpha(d: Double): Series = this.copy(shadowAlpha = Some(d))

  def fill(b: Boolean): Series = this.copy(fill = Some(b))

  def fillAndStroke(b: Boolean): Series = this.copy(fillAndStroke = Some(b))

  def fillColor(c: String): Series = this.copy(fillColor = Some(c))

  def fillAlpha(d: Double): Series = this.copy(fillAlpha = Some(d))

  def renderer(r: Renderer): Series = this.copy(renderer = Some(r))

  def rendererOptions(r: RenderOptions): Series = this.copy(rendererOptions = Some(r))

  def markerOptions(m: MarkerOption): Series = this.copy(markerOptions = Some(m))

  def color(c: String): Series = this.copy(color = Some(c))

  def fields: List[(String, Option[Any])] = List(("xaxis", xaxis), ("yaxis", yaxis),
    ("label", label),
    ("color", colour),
    ("lineWidth", lineWidth),
    ("showLine", displayLine),
    ("showShadow", showShadow),
    ("shadowAngle", shadowAngle),
    ("shadowOffset", shadowOffset),
    ("shadowWidth", shadowWidth),
    ("shadowDepth", shadowDepth),
    ("shadowAlpha", shadowAlpha),
    ("fill", fill),
    ("fillAndStroke", fillAndStroke),
    ("fillColor", fillColor),
    ("fillAlpha", fillAlpha),
    ("renderer", renderer),
    ("rendererOptions", rendererOptions),
    ("markerOptions", markerOptions),
    ("color", color))


  def toJObject = { JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t))) }

  override def toJson = {
    JField("seriesDefaults",
      JObject(for {
        b <- fields
        t <- b._2
      } yield {
        Box.asA[JSONable](t).map(_.toJson).openOr(JField(b._1, toJValue(t)))
      }))
  }

  override val possible_renderers = List(markerOptions, renderer)

}

sealed trait Location

case class NW() extends Location {override def toString = this.getClass.getSimpleName.toLowerCase()}

case class NO() extends Location {override def toString = "n"}

case class NE() extends Location {override def toString = this.getClass.getSimpleName.toLowerCase()}

case class EA() extends Location {override def toString = "e"}

case class SE() extends Location {override def toString = this.getClass.getSimpleName.toLowerCase()}

case class SO() extends Location {override def toString = "s"}

case class SW() extends Location {override def toString = this.getClass.getSimpleName.toLowerCase()}

case class WE() extends Location {override def toString = "w"}

//Missing show. Assumption, if you don't want to show the legend, don't include it.
case class Legend(display: Option[Boolean] = None, location: Option[Location] = None, xoffset: Option[Int] = None, yoffset: Option[Int] = None) extends JSONable {

  def show: Legend = this.copy(display = Some(true))

  def hide: Legend = this.copy(display = Some(false))

  def location(l: Location): Legend = this.copy(location = Some(l))

  def xoffset(x: Int): Legend = this.copy(xoffset = Some(x))

  def yoffset(y: Int): Legend = this.copy(yoffset = Some(y))

  private def fields: List[(String, Option[Any])] = List(("show", display), ("location", location), ("xoffset", xoffset), ("yoffset", yoffset))

  override def toJson = { JField("legend", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

}

case class Grid(drawGridLines: Option[Boolean] = None,
                gridLineColor: Option[String] = None,
                background: Option[String] = None,
                borderColor: Option[String] = None,
                borderWidth: Option[Double] = None,
                renderer: Option[Renderer] = None,
                rendererOptions: Option[RenderOptions] = None,
                showShadow: Option[Boolean] = None,
                shadowAngle: Option[Int] = None,
                shadowOffset: Option[Double] = None,
                shadowWidth: Option[Int] = None,
                shadowDepth: Option[Int] = None,
                shadowAlpha: Option[Double] = None) extends JSONable with Renderable {

  def drawGridLines(b: Boolean): Grid = this.copy(drawGridLines = Some(b))

  def gridLineColor(s: String): Grid = this.copy(gridLineColor = Some(s))

  def background(s: String): Grid = this.copy(background = Some(s))

  def borderColor(s: String): Grid = this.copy(borderColor = Some(s))

  def borderWidth(d: Double): Grid = this.copy(borderWidth = Some(d))

  def renderer(r: Renderer): Grid = this.copy(renderer = Some(r))

  def rendererOptions(r: RenderOptions): Grid = this.copy(rendererOptions = Some(r))

  def showShadow(b: Boolean): Grid = this.copy(showShadow = Some(b))

  def shadowAngle(i: Int): Grid = this.copy(shadowAngle = Some(i))

  def shadowOffset(d: Double): Grid = this.copy(shadowOffset = Some(d))

  def shadowWidth(i: Int): Grid = this.copy(shadowWidth = Some(i))

  def shadowDepth(i: Int): Grid = this.copy(shadowDepth = Some(i))

  def shadowAlpha(d: Double): Grid = this.copy(shadowAlpha = Some(d))


  private val fields: List[(String, Option[Any])] = List(("drawGridLines", drawGridLines),
    ("gridLineColor", gridLineColor),
    ("background", background),
    ("borderWidth", borderWidth),
    ("shadow", showShadow),
    ("shadowAngle", shadowAngle),
    ("shadowOffset", shadowOffset),
    ("shadowDepth", shadowDepth),
    ("shadowWidth", shadowWidth),
    ("shadowAlpha", shadowAlpha),
    ("renderer", renderer),
    ("rendererOptions", rendererOptions))


  override def toJson = { JField("grid", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

  override val possible_renderers = List(renderer)

}

case class HighLighter(show: Option[Boolean] = None) extends JSONable with Renderable {

  def display: HighLighter = this.copy(show = Some(true))

  def hide: HighLighter = this.copy(show = Some(false))

  private val renderer = Some(HighLighterRenderer())

  private val fields: List[(String, Option[Any])] = List(("show", show))

  def toJObject = JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))

  override def toJson = { JField("highlighter", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

  override val possible_renderers = List(renderer)

}

case class Cursor(show: Option[Boolean] = None, tooltipLocation: Option[Location] = None) extends JSONable with Renderable {

  private val renderer = Some(CursorRenderer())

  def display: Cursor = this.copy(show = Some(true))

  def hide: Cursor = this.copy(show = Some(false))

  def tooltipLocation(l: Location): Cursor = this.copy(tooltipLocation = Some(l))

  private val fields: List[(String, Option[Any])] = List(("show", show), ("tooltipLocation", tooltipLocation))

  def toJObject = JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))

  override def toJson = { JField("cursor", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

  override val possible_renderers = List(renderer)

}

case class LabelOptions(fontFamily: Option[String] = None,
                        fontSize: Option[String] = None) extends JSONable {

  private val fields: List[(String, Option[Any])] = List(("fontFamily", fontFamily), ("fontSize", fontSize))

  override def toJson = { JField("labelOptions", JObject(for {b <- fields; t <- b._2} yield JField(b._1, toJValue(t)))) }

}

