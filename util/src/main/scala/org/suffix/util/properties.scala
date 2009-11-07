package org.suffix.util

object Properties {
    import scala.io.{Source,BufferedSource}
    import scala.xml.XML
    import java.io.PrintStream
    
    case class NotFound(s: String) extends Exception(s)
  
    def getProperties = this.getClass getResourceAsStream "/application.properties"
    val st = getProperties
          
    // in order to reset, do 
    // var ss = resettable; ss = ss.reset
    // and use the newly assigned ss, not old one!
    
    def resettable: BufferedSource =
      new BufferedSource(st)

      
    def lines = resettable.getLines _
  
    // val twitterUser=linesIter.next.trim
    // can use some simple steganography here
    // val twitterPassword=linesIter.next.trim
    
    // NB: load lazily, on demand from get
    val x = XML.load(st)

    
    // NB how can we make it get[T]...: T 
    // so e.g. it'd return Int by doing toInt if needed?
    
    def get(name: String): String = {
      val ns = x \ name
      
      if (ns.length == 0) throw NotFound(name+" absent")
      
      ns.text
    }

    def getString(name: String, default: String): String = {
      try {
        get(name)
      } catch {
        case NotFound(_) => default
      }
    }
        
    def getInt(name: String, default: Int): Int = {
      try {
        val x = get(name)
        x.toInt
      } catch {
        case NotFound(_) => default
        case _: NumberFormatException => default
      }
    }
    
    def getLong(name: String, default: Long): Long = {
      try {
        val x = get(name)
        x.toLong
      } catch {
        case NotFound(_) => default
        case _: NumberFormatException => default
      }
    }
    
    // one of the aux. versions with reporting
    def default[T](out: PrintStream, msg: String, name: String, i: T): T = { out.println(msg+" using default: "+name+" = "+i); i }
}