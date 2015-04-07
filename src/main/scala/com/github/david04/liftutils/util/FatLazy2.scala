package com.github.david04.liftutils.util

import net.liftweb.util.FatLazy

class FatLazy2[T](f: => T) extends FatLazy[T](f) {

  def apply() = get
}

object FatLazy2 extends scala.AnyRef {
  def apply[T](f: => T) = new FatLazy2[T](f)
}
