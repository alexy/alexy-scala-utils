package org.suffix.util.input

// the new one handles the leading BZ from command-line bzip2 automatically for us 
import java.util.zip.GZIPInputStream
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.{BufferedReader,InputStreamReader,FileInputStream}
import scala.io.Source
import System.err

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
  
  // TODO verbose flag would print the current file name to stderr;
  // alternatively, the holder of the files list can monitor line numbers with glueFilesLines below
  // and advance to the next fileName himself upon seeing line number 0
  def glueFilesLines(files: Array[String], verbose: Boolean = false): Iterator[String] = 
    files.iterator.flatMap { fileName => 
      if (verbose) err.println("reading "+fileName)
      sourceFile(fileName).getLines() 
    }
 
  def glueFilesLineNums(files: Array[String], verbose: Boolean = false): Iterator[(String,Int)] =
    files.iterator.flatMap { fileName => sourceFile(fileName).getLines().zipWithIndex }
}
