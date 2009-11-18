package org.suffix.util.input

// the new one handles the leading BZ from command-line bzip2 automatically for us 
import java.util.zip.GZIPInputStream
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.{BufferedReader,InputStreamReader,FileInputStream}
import scala.io.Source


// import scala.tools.nsc.io.File
// arr.iterator.flatMap( File(_).lines() )


object GlueSources {

  def sourceFile(fileName: String): Source = {
    val inStream: java.io.InputStream = { val fis = new FileInputStream(fileName)
      if (fileName.endsWith(".bz2")) 
        new BZip2CompressorInputStream(fis) // buffered inside!
      else if (fileName.endsWith(".gz"))
        new GZIPInputStream(fis)
      else fis }
    val source = Source.fromInputStream(inStream)
    // alternatively, 
    // val bread = new BufferedReader(new InputStreamReader(inStream, "UTF-8"))
    // http://viewfromthefringe.blogspot.com/2007/10/making-bufferedreader-iterable.html
    source
  }
  
  def glueFilesLines(files: Array[String]): Iterator[String] = 
    files.iterator.flatMap { fileName => sourceFile(fileName).getLines() }
 
  def glueFilesLineNums(files: Array[String]): Iterator[(String,Int)] =
    files.iterator.flatMap { fileName => sourceFile(fileName).getLines().zipWithIndex }
}
