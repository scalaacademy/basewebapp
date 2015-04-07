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

trait Framework {

  def errorClass: String
  def warningClass: String
  def successClass: String

  def btnDefault: String
  def btnMute: String
  def btnPrimary: String
  def btnSuccess: String
  def btnInfo: String
  def btnWarning: String
  def btnDanger: String
}

trait Bootstrap3 extends Framework {

  def fw: Framework = this

  def errorClass = "has-error"
  def warningClass = "has-warning"
  def successClass = "has-success"

  def btnDefault: String = "btn-default"
  def btnMute: String = "btn-default"
  def btnPrimary: String = "btn-primary"
  def btnSuccess: String = "btn-success"
  def btnInfo: String = "btn-info"
  def btnWarning: String = "btn-warning"
  def btnDanger: String = "btn-danger"
}

trait Bootstrap2 extends Framework {

  def fw: Framework = this

  def errorClass = "error"
  def warningClass: String = ???
  def successClass: String = ???

  def btnDefault: String = ""
  def btnMute: String = ""
  def btnPrimary: String = "btn-primary"
  def btnSuccess: String = "btn-success"
  def btnInfo: String = "btn-info"
  def btnWarning: String = "btn-warning"
  def btnDanger: String = "btn-danger"
}