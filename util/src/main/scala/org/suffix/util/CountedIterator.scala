// CountedIterator was fixed by @paulp then dropped from 2.8
// here's @SethTisue's rendition:
// http://paste.pocoo.org/raw/134205/

package org.suffix.util

class CountedIterator[T](it: Iterator[T]) {
  var n = 0
  def count = n
  def hasNext = it.hasNext
  def next() = {
    val result = it.next()
    n += 1
    result
  }
}

class RichIterator[T](it: Iterator[T]) {
  def withCounting = new CountedIterator(it)
}

object types {
  implicit def pimpMyIterator[T](it: Iterator[T]): RichIterator[T] =
  new RichIterator(it)
  
  def main(args: Array[String]) = {
 //   val it = List.make(5,"foo").elements.withCounting // 2.7
    val it = List.fill(5)("foo").iterator.withCounting  // 2.8
    println(it.count)
    it.next()
    it.next()
    println(it.count)
  }
}

