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

import com.github.david04.liftutils.loc.LocP

object TimeSince extends LocP {

  val scales = List((1000L, loc("milli")), (60L, loc("second")), (60L, loc("minute")), (24L, loc("hour")), (7L, loc("day")), (10000L, loc("week")))

  def apply(millis: Long): String = {
    def divideInUnits(millis: Long) = scales.foldLeft[(Long, List[(Long, String)])]((millis, Nil)) {
      (total, div) =>
        (total._1 / div._1, (total._1 % div._1, div._2) :: total._2)
    }._2
    def formatAmount(amountUnit: (Long, String)) = amountUnit match {
      case (amount, unit) => amount + " " + unit
    }
    divideInUnits(millis).filter(_._1 > 0).map(formatAmount(_)).mkString(", ")
  }
}