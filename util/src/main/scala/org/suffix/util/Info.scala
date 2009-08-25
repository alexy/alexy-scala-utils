package org.suffix.util

import System.err

object Info {
  val info = System.err.println(_ : Any)
}

object Debug {
  def print(show: Boolean)(a: Any): Unit   = if (show) err.print(a)
  def println(show: Boolean)(a: Any): Unit = if (show) err.println(a)
}