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

import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{InMemoryResponse, Req}

/**
 * Created by david at 9:37 PM
 */
object VirtualFiles extends RestHelper {

  val files = collection.mutable.Map[String, String]()

  def name(file: String) = ("/js/" + ## + file)

  def ext(name: String) = Map(
    "js" -> "text/javascript"
  ).get(name).getOrElse("application/octet-stream")

  serve {
    case Req("js" :: name :: Nil, suf, _) if (files.contains((name + "." + suf).replaceAllLiterally(## + "", ""))) =>
      val bytes = files((name + "." + suf).replace(## + "", "")).getBytes("UTF-8")
      InMemoryResponse(
        bytes,
        ("Content-Length", bytes.length.toString) ::
          ("Content-Type", ext(suf) + "; charset=utf-8") ::
          Nil,
        Nil,
        200)
  }
}
