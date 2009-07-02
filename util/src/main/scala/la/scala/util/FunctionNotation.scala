package org.suffix.util
// la.scala.util bombs in Scala 2.7.5 here,
// while it works fine in Scala 2.8.0-SNAPSHOT!

object FunctionNotation {
  case class Function1WithLeftInput[-T1,+R](f: T1 => R) {
    def ->:(input: T1) = f(input)
    def ->:[A](e: A => T1) = (e andThen f)
  }

  implicit def function1ToLeftInput[T1,R](f: T1 => R) =
    Function1WithLeftInput(f)
}
