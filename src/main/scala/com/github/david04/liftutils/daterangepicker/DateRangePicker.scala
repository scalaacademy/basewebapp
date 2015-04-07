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

package com.github.david04.liftutils.daterangepicker

import com.github.david04.liftutils.jacksonxml.{JSON, JsonSerializable}
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml, Templates}
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._

import scala.collection.SortedMap

case class DateRangePicker(
                            onSelection: (Long, Long) => JsCmd,
                            options: Option[Options] = None
                            ) {

  val template = Templates("templates-hidden" :: "daterangepicker-dflt" :: Nil).get

  implicit val formats = net.liftweb.json.DefaultFormats

  val id = S.formFuncName

  val customRanges = (
    for {
      o <- options.toSeq
      rs <- o.ranges.toSeq
      r <- rs.ranges.collect({case c: CustomRange => c})
    } yield (r.id, r)
    ).toMap

  def setCustomRange(range: CustomRange) = "$('#" + id + " .daterangepicker-lbl').text(" + range.name.encJs + ");"

  def render =
    (
      ".daterangepicker-around [id]" #> id &
        ".daterangepicker-around [style]" #> "display: block;"
      ).apply(template) ++
      <tail>
        {Script(OnLoad(Run("$('#" + id + "').daterangepicker(" +
        JSON.L.writeValueAsString(options) + ",\n" +
        "function (_start, _end) {" +
        "var start = _start.toDate().getTime();" +
        "var end = _end.toDate().getTime();" +
        "if(start > 0 && end > 0) {" +
        (customRanges.values.toList.map(r => s"if(start == end && start == ${r.id}) " + setCustomRange(r)) :+
          "{$('#" + id + " .daterangepicker-lbl').html(_start.format('MMMM D, YYYY') + ' - ' + _end.format('MMMM D, YYYY'));}")
          .mkString(" else ") +
        SHtml.jsonCall(
          JsRaw("[start,end]"),
          (v: JValue) => v match {
            case JArray((JInt(from)) :: (JInt(to)) :: Nil) =>
              if (from == to && customRanges.isDefinedAt(from.toLong)) customRanges(from.toLong).onSelection.apply()
              else onSelection(from.toLong, to.toLong)
            case _ => ???
          }).toJsCmd +
        "}" +
        "}).data('daterangepicker').notify();"

      ) & JsShowId(id)))}
      </tail>
}

trait RangeOption {
  def name: String

  def value: (Moment, Moment)
}

case class Range(name: String, from: Moment, to: Moment) extends RangeOption {
  def value: (Moment, Moment) = (from, to)
}

case class CustomRange(name: String, onSelection: () => JsCmd) extends RangeOption {
  val idValue = (99999999999999L - math.abs(name.hashCode).toLong * 10000)
  val id = idValue - idValue % (24 * 60 * 60 * 1000l)

  def value: (Moment, Moment) = (new Moment(id), new Moment(id))
}

case class Ranges(ranges: RangeOption*) extends JsonSerializable {
  def json(): Option[String] =
    Some(JSON.L.writeValueAsString(SortedMap(ranges.map(r => (r.name, r.value)): _*)(Ordering.by(v => ranges.indexWhere(_.name == v)))))
}

case class Locale(
                   applyLabel: Option[String] = None,
                   fromLabel: Option[String] = None,
                   toLabel: Option[String] = None,
                   customRangeLabel: Option[String] = None,
                   daysOfWeek: Option[Array[String]] = None,
                   monthNames: Option[Array[String]] = None,
                   firstDay: Option[Int] = None
                   )

case class Options(
                    opens: Option[String] = None,
                    startDate: Option[Moment] = None,
                    endDate: Option[Moment] = None,
                    minDate: Option[String] = None,
                    maxDate: Option[String] = None,
                    //dateLimit
                    showDropdowns: Option[Boolean] = None,
                    showWeekNumbers: Option[Boolean] = None,
                    timePicker: Option[Boolean] = None,
                    timePickerIncrement: Option[Int] = None,
                    timePicker12Hour: Option[Boolean] = None,
                    ranges: Option[Ranges] = None,
                    buttonClasses: Option[Array[String]] = None,
                    applyClass: Option[String] = None,
                    cancelClass: Option[String] = None,
                    format: Option[String] = None,
                    separator: Option[String] = None,
                    locale: Option[Locale] = None
                    )

object Moment {

  object Time extends Enumeration {
    val years = Value("years")
    val months = Value("months")
    val days = Value("days")
    val hours = Value("hours")
    val minutes = Value("minutes")
    val seconds = Value("seconds")
    val milliseconds = Value("milliseconds")
  }

}

case class Moment private(cmd: String) extends JsonSerializable {

  def json = Some(cmd)

  def this(millis: Long) = this(s"moment($millis)")

  def this() = this(s"moment()")

  def subtract(amount: Int, unit: Moment.Time.Value) = Moment(s"$cmd.subtract('$unit', $amount)")

  def startOf(unit: Moment.Time.Value) = Moment(s"$cmd.startOf('$unit')")

  def endOf(unit: Moment.Time.Value) = Moment(s"$cmd.endOf('$unit')")
}