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

package com.github.david04.liftutils.flot

import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

case class ChartTooltip(offsetX: Int, offsetY: Int, tooltips: Seq[(NodeSeq, JsCmd)])

case class Chart(
                  series: Array[Series],
                  options: Option[Options.Options] = None,
                  tooltip: Option[ChartTooltip] = None
                  ) {

  implicit val formats = net.liftweb.json.DefaultFormats

  val id = S.formFuncName

  def render = try {
    <div id={id} style="width: 100%; height: 300px;"></div> ++
      <tail>{
        Script(OnLoad(Run({
          "window.plot" + id + " = $('#" + id + "').plot(" +
            compact(JsonAST.render(decompose(series))) +
            options.map(opt => compact(JsonAST.render(decompose(opt)))).map("," + _).getOrElse("") +
            ").data('plot');" +
            tooltip.map(tp => {
              val idSel = "$('#" + id + "')"
              val tpId = id + "tt"
              val tpSel = "$('#" + tpId + "')"
              val d = "$"
              val html = """'<div class="chart-tooltip"><div class="date">' + 'hello' + '<\div><\div>'"""
              val tooltips = tp.tooltips.map(_._1.toString.encJs).mkString("[", ",", "]")
              val tooltipsJs = tp.tooltips.map(_._2.toJsCmd.encJs).mkString("[", ",", "]")
              s"""
                |var tooltips = $tooltips;
                |var tooltipsJs = $tooltipsJs;
                |(function(){
                |var previousPoint = null;
                |$idSel.bind('plothover', function (event, pos, item) {
                |if (item) {
                |  if (previousPoint != item.dataIndex) {
                |    previousPoint = item.dataIndex;
                |    $tpSel.remove();
                |    var x = item.pageX;
                |    var y = item.pageY;
                |    $d(tooltips[item.dataIndex]).css({
                |      position: 'absolute',
                |      display: 'none',
                |      top: y + ${tp.offsetY},
                |      left: x + ${tp.offsetX}
                |    }).attr('id', '$tpId').appendTo("body").fadeIn(200);
                |    eval('0, ' + tooltipsJs[item.dataIndex]);
                |  }
                |} else {
                |  $tpSel.remove();
                |  previousPoint = null;
                |}
                |});
                |})();"""
            }.stripMargin).getOrElse("")
        })))
      }</tail>
  } catch {
    case e: Exception =>
      e.printStackTrace()
      throw e
  }
}

case class Series(
                   label: String,
                   data: JArray
                   )

object Options {

  implicit def toOpt[T](v: T) = Some(v)

  def dashboardTimeBasedChart(from: Long, to: Long): Option[Options] = Options(
    series = Series(
      lines = Lines(
        show = true,
        lineWidth = 2,
        fill = true,
        fillColor = FillColor(
          colors = Array(
            Color(opacity = 0.05),
            Color(opacity = 0.01)
          )
        )
      ),
      points = Points(
        show = true
      ),
      shadowSize = 2
    ),
    grid = Grid(
      hoverable = true,
      clickable = true,
      tickColor = "#eee",
      borderWidth = 0
    ),
    colors = Array("#d12610", "#37b7f3", "#52e136"),
    xaxis = Axis(
      mode = "time",
      ticks = 11,
      tickDecimals = 0,
      min = from,
      max = to
    ),
    yaxis = Axis(
      ticks = 11,
      tickDecimals = 0
    )
  )

  //  {
  //    color: color or number
  //    data: rawdata
  //    label: string
  //    lines: specific lines options
  //    bars: specific bars options
  //    points: specific points options
  //    xaxis: number
  //    yaxis: number
  //    clickable: boolean
  //    hoverable: boolean
  //    shadowSize: number
  //    highlightColor: color or number
  //  }
  case class Options(
                      series: Option[Series] = None,
                      grid: Option[Grid] = None,
                      colors: Option[Array[String]] = None,
                      xaxis: Option[Axis] = None,
                      yaxis: Option[Axis] = None
                      )

  case class Axis(
                   /**
                    * "time" for time
                    */
                   mode: Option[String] = None,
                   ticks: Option[Int] = None,
                   tickDecimals: Option[Int] = None,
                   min: Option[Long] = None,
                   max: Option[Long] = None
                   )

  case class Grid(
                   hoverable: Option[Boolean] = None,
                   clickable: Option[Boolean] = None,
                   tickColor: Option[String] = None,
                   borderWidth: Option[Int] = None
                   )

  case class Series(
                     lines: Option[Lines] = None,
                     points: Option[Points] = None,
                     shadowSize: Option[Int] = None
                     )

  case class Lines(
                    show: Option[Boolean] = None,
                    lineWidth: Option[Int] = None,
                    fill: Option[Boolean] = None,
                    fillColor: Option[FillColor] = None
                    )

  case class Points(
                     show: Option[Boolean] = None,
                     lineWidth: Option[Int] = None,
                     fill: Option[Boolean] = None,
                     fillColor: Option[FillColor] = None
                     )

  case class FillColor(
                        colors: Option[Array[Color]]
                        )

  case class Color(opacity: Option[Double])

}

object Main extends App {


  implicit val formats = net.liftweb.json.DefaultFormats

  //  val json = ("name" -> "joe") ~ ("age" -> 35)
  implicit def toJArray(v: (Int, Int)) = JArray(JInt(v._1) :: JInt(v._2) :: Nil)

  //  println(pretty(render(decompose(
  //    Series("test", (0 -> 2) :: (1 -> 3) :: (2 -> 2) :: Nil)
  //  ))))
}