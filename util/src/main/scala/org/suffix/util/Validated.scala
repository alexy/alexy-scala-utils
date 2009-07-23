package org.suffix.util
// from Mark McBride we borrow this hunk of code:

trait Validated{
  def isValid(): Boolean
}
