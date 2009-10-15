package org.suffix.util

object JavaScalaIter {
	implicit def javaScalaIter[A](j: java.util.Iterator[A]): Iterator[A] = {
		new Iterator[A] {
		  def hasNext = j.hasNext
		  def next = j.next
		}
	}

	def main(args: Array[String]) {
	
		// java.util.Arrays.asList(1, 2, 3).iterator
		// java.lang.ClassCastException: [I cannot be cast to [Ljava.lang.Object;
		
		val jter = java.util.Arrays.asList("1", "2", "3").iterator
		
		for (t <- jter) println(t)
	}
}
