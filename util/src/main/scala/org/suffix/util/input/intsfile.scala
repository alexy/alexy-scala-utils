package org.suffix.util.input

import java.io.{FileReader,BufferedReader}

object File {
  def bufReader(fileName: String) =
    new BufferedReader(new FileReader(fileName))  
}

object Ints {
  // TODO official getter and setter?
  var sep: String = ","
  def setSeparator(s: String) = sep = s
  def getSeparator: String = sep
  
  def getPair(s: String): (Int, Int) = {
    // TODO catch matching exception here 
    // for wrong format clarification?
    val Seq(a,b) = getArray(s)
    (a,b)
  }

  type IntPairs = List[(Int,Int)]
  def readPairs(fileName: String): IntPairs = {
    val bufr = File.bufReader(fileName)
    def readPairsAcc(acc: IntPairs): IntPairs = {
      val line = bufr.readLine
      if (line == null) {
        bufr.close
        acc.reverse
      }
      else {
        val pair = getPair(line)
        readPairsAcc(pair::acc)
      } 
    }
    readPairsAcc(Nil)
  }
  
  def getArray(s: String): Array[Int] = {
    s.trim.split(sep) map (_.toInt)
  }
  
  def getList(s: String): List[Int] = {
    getArray(s).toList
  }

  type IntsList = List[List[Int]]
  
  def readLists(fileName: String): IntsList = {
    val bufr = File.bufReader(fileName)
    def readListsAcc(acc: IntsList): IntsList = {
      val line = bufr.readLine
      if (line == null) {
        bufr.close
        acc.reverse
      }
      else {
        val list = getList(line)
        readListsAcc(list::acc)
      } 
    }
    readListsAcc(Nil)
  } 
}