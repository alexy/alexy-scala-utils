// @n8han after http://scala.sygneca.com/code/simplifying-jdbc
// @khrabrov package, de-;-ed

package org.suffix.sql.rich

import java.sql.{DriverManager, Connection, ResultSet, PreparedStatement, Statement, Date, Timestamp}
import org.joda.time.DateTime

object RichSQL {
    implicit def rrs2Boolean(rs: RichResultSet) = rs.nextBoolean
    implicit def rrs2Byte(rs: RichResultSet) = rs.nextByte
    implicit def rrs2Int(rs: RichResultSet) = rs.nextInt
    implicit def rrs2Long(rs: RichResultSet) = rs.nextLong
    implicit def rrs2LongOption(rs: RichResultSet) = rs.nextLongOption
    implicit def rrs2Float(rs: RichResultSet) = rs.nextFloat
    implicit def rrs2Double(rs: RichResultSet) = rs.nextDouble
    implicit def rrs2String(rs: RichResultSet) = rs.nextString
    implicit def rrs2Date(rs: RichResultSet) = rs.nextDate
    implicit def rrs2Timestamp(rs: RichResultSet) = rs.nextTimestamp
    implicit def rrs2DateTime(rs: RichResultSet) = new DateTime(rs.nextTimestamp.getTime)
    implicit def st2Rich(s: Statement) = new RichStatement(s)
    implicit def rich2St(rs: RichStatement) = rs.s

    implicit def rich2ResultSet(r: RichResultSet) = r.rs
    implicit def resultSet2Rich(rs: ResultSet) = new RichResultSet(rs)
    implicit def query[X](s: String, f: RichResultSet => X)(implicit stat: Statement) = {
        strm(f,stat.executeQuery(s))
    }
    implicit def conn2Rich(conn: Connection) = new RichConnection(conn)
    implicit def rich2PS(r: RichPreparedStatement) = r.ps
    implicit def ps2Rich(ps: PreparedStatement) = new RichPreparedStatement(ps)
    implicit def str2RichPrepared(s: String)(implicit conn: Connection)     :RichPreparedStatement = conn prepareStatement(s)

    private def strm[X](f: RichResultSet => X, rs: ResultSet): Stream[X] = 
        if (rs.next) Stream.cons(f(new RichResultSet(rs)), strm(f, rs))
        else { rs.close(); Stream.empty }
    
    implicit def conn2Statement(conn: Connection): Statement = conn.createStatement
    
 
    class RichResultSet(val rs: ResultSet) {
        
      var pos = 1
      def apply(i: Int) = { pos = i; this }
      private def inc = { pos += 1 }
  
      def nextBoolean: Boolean = { val r = rs.getBoolean(pos); inc; r }
      def nextByte: Byte = { val r = rs.getByte(pos); inc; r }
      def nextInt: Int = { val r = rs.getInt(pos); inc; r }
      def nextLong: Long = { val r = rs.getLong(pos); inc; r }
      // played with the following in the clients:
      // def getOrNull(o: Option[_]) = o match { case Some(x) => x; case _ => null }
      // then dug in and did an option for each type needed:
      def nextLongOption: Option[Long] = { 
        // Any/cast not necessary, JDBC returns 0 for null anyways:
        // val grab: Any = rs.getLong(pos)
        // ... else Some(grab.asInstanceOf[Long])
        val grab: Long = rs.getLong(pos)
        val r: Option[Long] = if (rs.wasNull) None else Some(grab)
        inc
        r
        }
      def nextFloat: Float = { val r = rs.getFloat(pos); inc; r }
      def nextDouble: Double = { val r = rs.getDouble(pos); inc; r }
      def nextString: String = { val r = rs.getString(pos); inc; r }
      def nextDate: Date = { val r = rs.getDate(pos); inc; r }
      def nextTimestamp: Timestamp = { val r = rs.getTimestamp(pos); inc; r }
      def nextDateTime: DateTime = new DateTime(nextTimestamp)
 
      def foldLeft[X](init: X)(f: (ResultSet, X) => X): X = rs.next match {
        case false => init
        case true => foldLeft(f(rs, init))(f)
      }
      def map[X](f: ResultSet => X) = {
        var ret = List[X]()
        while (rs.next())
        ret = f(rs) :: ret
        ret.reverse // ret should be in the same order as the ResultSet
      }
    }
 
    class RichPreparedStatement(val ps: PreparedStatement) {
      var pos = 1
      private def inc = { pos = pos + 1; this }
 
      def execute[X](f: RichResultSet => X): Stream[X] = {
	      pos = 1; strm(f, ps.executeQuery)
      }
      def <<![X](f: RichResultSet => X): Stream[X] = execute(f)
 
      def execute = { pos = 1; ps.execute }
      def <<! = execute
 
      def <<(b: Boolean) = { ps.setBoolean(pos, b); inc }
      def <<(x: Byte) = { ps.setByte(pos, x); inc }
      def <<(i: Int) = { ps.setInt(pos, i); inc }
      def <<(x: Long) = { ps.setLong(pos, x); inc }
      def <<(xo: Option[Long]) = { 
        xo match {
          case Some(x) => ps.setLong(pos, x)
          case _ => ps.setNull(pos,java.sql.Types.BIGINT)
        }
        inc
      }
      def <<(f: Float) = { ps.setFloat(pos, f); inc }
      def <<(d: Double) = { ps.setDouble(pos, d); inc }      
      def <<(o: String) = { ps.setString(pos, o); inc }
      def <<(x: Date) = { ps.setDate(pos, x); inc }
      // how do we help Scala infer the natural:
      // def <<(x: DateTime) = <<(x.toDate)
      def <<(x: DateTime) = { ps.setTimestamp(pos, new java.sql.Timestamp(x.getMillis)); inc }
    }
    
    class RichConnection(val conn: Connection) {
      def <<(sql: String) = new RichStatement(conn.createStatement) << sql
      def <<(sql: Seq[String]) = new RichStatement(conn.createStatement) << sql
    }
    
    class RichStatement(val s: Statement) {
      def <<(sql: String) = { s.execute(sql); this }
      def <<(sql: Seq[String]) = { for (x <- sql) s.execute(x); this }
    }

  // extract a single result from the stream, when present
  // unchecked: Some(e: T) will not be checked due to type erasure
  def deStream[T] = { x: Any =>
    x match { case Stream(e: T) => Some(e: T); case _ => None }
  }

}
