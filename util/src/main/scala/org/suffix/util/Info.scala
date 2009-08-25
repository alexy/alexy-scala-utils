package org.suffix.util

import System.err

object Info {
  val info = System.err.println(_ : Any)
}

object Debug {
  def print(show: Boolean)(s: String): Unit   = if (show) err.print(s)
  def println(show: Boolean)(s: String): Unit = if (show) err.println(s)
}