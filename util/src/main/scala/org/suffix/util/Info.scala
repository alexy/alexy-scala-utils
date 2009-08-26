package org.suffix.util
import scala.collection.mutable.{Map=>MMap,Set=>SSet}
import System.err

// off-topic, from @paulp:
// List(⚀, ⚁, ⚂, ⚃, ⚄, ⚅)

object Debug {
  def print(show: Boolean)(a: Any): Unit   = if (show) err.print(a)
  def println(show: Boolean)(a: Any): Unit = if (show) err.println(a)
}

object Info {
  val say = err.println(_ : Any)

  var shows: MMap[Symbol,Int] = MMap.empty
  var groups: MMap[Symbol,SSet[Symbol]] = MMap.empty
    
  var showAll = false
  var defaultLevel = 1
  
  def set(s: Symbol) = shows(s) = defaultLevel
  def set(s: Symbol, level: Int) = shows(s) = level
  def unset(s: Symbol) = shows(s) = 0
  def reset = shows = MMap.empty
  var reportLevel = 1
  
  def group(g: Symbol, list: List[Symbol]) = (groups get g) match {
    case Some(_) => list foreach { s => groups(g) += s }
    case _ => groups(g) = SSet(list: _*)
  }
  
  def group(g: Symbol, s: Symbol) = (groups get g) match {
    case Some(_) => groups(g) += s
    case _ => groups(g) = SSet(s)
  }
  
  def enableGroup(g: Symbol, level: Int) = groups(g) foreach set
  def disableGroup(g: Symbol) = groups(g) foreach unset
  
  def info(s: Symbol)(a: Any) = 
    if (showAll) say(a)
    else 
    (shows get s) match {
      case Some(x) if (x >= reportLevel) => say(a)
      case _ => 
    }

  def symbols(s: String): List[Symbol] = s.split(',').toList map { Symbol(_) }
  
  def getSymbols(so: Option[String]): List[Symbol] = so match {
    case Some(s) => symbols(s)
    case _ => Nil
  }
}
