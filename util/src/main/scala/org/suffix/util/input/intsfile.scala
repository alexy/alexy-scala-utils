package org.suffix.util.input

import java.io.{FileReader,BufferedReader}

object File {
  def bufReader(fileName: String) =
    new BufferedReader(new FileReader(fileName))  
}

object Ints {
  // TODO official getter and setter?
  var sep1: String = ","
  var sep2: String = ";"
  def setSeparator1(s: String) = sep1 = s
  def getSeparator1: String = sep1
  def setSeparator2(s: String) = sep2 = s
  def getSeparator2: String = sep2
  
  def getPair(s: String): (Int, Int) = {
    // TODO catch matching exception here 
    // for wrong format clarification?
    val Seq(a,b) = getArray(s)
    (a,b)
  }
  
  // TODO do we really need toList, or Array will do?
  def getPairs(s: String): List[(Int,Int)] = {
    (s split sep2).toList map getPair
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
    s.trim.split(sep1) map (_.toInt)
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