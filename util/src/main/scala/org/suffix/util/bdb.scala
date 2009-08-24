package org.suffix.util.bdb

import System.err
import java.io.File
import com.sleepycat.je.{Environment,EnvironmentConfig,Transaction}
import com.sleepycat.persist.{EntityStore,StoreConfig,EntityCursor}

case class BdbFlags (
  allowCreate: Boolean,
  readOnly: Boolean,
  transactional: Boolean,
  deferredWrite: Boolean,
  noSync: Boolean
  )

case class BdbArgs (
  envPath: String,
  storeName: String,
  flags: BdbFlags,
  cacheSize: Option[Long]
  )

class BdbStore(bdbArgs: BdbArgs) {
  val BdbArgs(envPath,storeName,bdbFlags,cacheSize) = bdbArgs
  val BdbFlags(bdbAllowCreate,bdbReadOnly,bdbTransactional,bdbDeferredWrite,bdbNoSync) = bdbFlags
  
  err.println("BDB env "+envPath+", DPL store: "+storeName)
  /* Open the JE Environment. */
  val envConfig = new EnvironmentConfig

  if (bdbReadOnly) { envConfig.setReadOnly(true)
    err.println("BDB Env ReadOnly") }
  else if (bdbAllowCreate) { envConfig.setAllowCreate(true)
    err.println("BDB Env AllowCreate") }

  if (bdbTransactional) { envConfig.setTransactional(true)
    err.println("BDB Env Transactional") }
  // env has no setDeferredWrite
  
  cacheSize match {
    case Some(n) => { envConfig.setCacheSize(n)
      err.println("BDB setting cache size "+n) }
    case _ => err.println("BDB setting NO cache size, using default 60% of Xmx")
  }
  
  if (bdbNoSync) { envConfig.setTxnNoSync(true)
    err.println("BDB setting TxnNoSync") }

  val env = new Environment(new File(envPath), envConfig)

  /* Open the DPL Store. */
  val storeConfig = new StoreConfig

  if (bdbReadOnly) { storeConfig.setReadOnly(true)
    err.println("BDB Store ReadOnly") }
  else if (bdbAllowCreate) { storeConfig.setAllowCreate(true)
    err.println("BDB Store AllowCreate") }


  if (bdbTransactional) { storeConfig.setTransactional(true)
    err.println("BDB Store Transactional") }
  else if (bdbDeferredWrite) { storeConfig.setDeferredWrite(true)
    err.println("BDB Store DeferredWrite") }

  val store = new EntityStore(env, storeName, storeConfig)
  
  var txn: Transaction = null // get type for transaction

  // wrapping transactional calls in do or nothing semantics;
  // may instead explicitly say
  // if (bdbTransactional) txn.XXX in insertUserTwit
  
  def txnBegin: Unit = bdbTransactional match {
      case true => txn = env.beginTransaction(null, null)
      case _ =>
    }
  
  def txnCommit: Unit = bdbTransactional match {
    case true => txn.commit
    case _ =>
  }

  def txnRollback: Unit =  bdbTransactional match {
    case true => txn.abort
    case _ =>
  }

  def close: Unit = {
    try {
      // NB need to avoid commit if there's no txn active
      // txnCommit
      store.sync
      store.close
      env.sync
      env.close
    } catch {
      // if closing already closed, do nothing
      // or could check whether already closed -- how?
      // je.DatabaseException if cursors are not closed, closing them
      case e: IllegalStateException => err.println(e)
      case e: com.sleepycat.je.DatabaseException => err.println(e)
    }
  }
  
  // Stream.continually is in 2.8
  def continually[A](elem: => A): Stream[A] = Stream.cons(elem, continually(elem))
  // paulp has this in scala.util.control.Exception in 2.8:
  // def ultimately[T](body: => Unit): Catch[T] = noCatch andFinally body
  def ultimately[T](fin: => Unit)(body: => T): T = try { body } finally { fin }

  // original version of takeWhileFinally after the takeWhile in 2.8, with if's
  // def takeWhileFinally[T](st: Stream[T], p: T => Boolean, fin: => Unit): Stream[T] =
  //  if (!st.isEmpty && p(st.head)) Stream.cons(st.head, takeWhileFinally(st.tail,p,fin))
  //  else { fin; Stream.empty }

  // braver version with match and extractor for head,tail
  def takeWhileFinally[T](st: Stream[T], p: T => Boolean, fin: => Unit): Stream[T] =
    st match {
      case Stream.cons(head,tail) if p(head) => Stream.cons(head, takeWhileFinally(tail,p,fin))
      case _ => fin; Stream.empty
    }
 
  // this doesn't work, closing after first elem -- mixed side-effects! @dibblego
  // def takeWhileFinally[T](st: Stream[T], p: T => Boolean, fin: => Unit): Stream[T] =
  //   { val r = st takeWhile p; fin; r }

  // com.sleepycat.je.DatabaseException: (JE 3.3.82) Cursor has been closed
  // def cursorStream[T](cursor: EntityCursor[T]): Stream[T] =
  //   ultimately(cursor.close){ // closes right after head, tail-lazy Stream!
  //     continually(cursor.next _) map (_.apply()) takeWhile (_ != null) force.toStream //}
  // @paulp takes 2 & 3:
  // error on append (() => cursor.close), or Stream(cursor.close _)
  //   (continually(cursor.next _) takeWhile (_ != null)) append (() => cursor.close) map (_.apply())
  //   (continually(cursor.next _) takeWhile (_ != null)) ++ Stream(cursor.close _) map (_.apply())

  // could use the line above without Finally, do it in caller allUserStats below?

  def cursorStream[T](cursor: EntityCursor[T]): Stream[T] =
    takeWhileFinally(continually(cursor.next _) map (_.apply()), (_:T) != null, cursor.close)

  def cursorIter[T](cursor: EntityCursor[T])(f: T => Unit): Unit = {
      val x = cursor.next
      if (x == null) {
          cursor.close
      } else {
          f(x)
          cursorIter(cursor)(f) // tail recursion
      }
  }
  
  class CursorIterator[T](cursor: EntityCursor[T]) extends Iterator[T] {
    var ahead: T = cursor.next
    def hasNext = ahead != null
    def next = {
      val ret = ahead
      ahead = cursor.next
      ret
    }
  }
  
    
  def cursorMap[T,R](cursor: EntityCursor[T])(f: T => R): List[R] = {  
    def cursorMapAux(cursor: EntityCursor[T])(res: List[R]): List[R] = {
      val x = cursor.next
      if (x == null) {
          cursor.close
          res // or res.reverse if you care about the order
      } else {
          cursorMapAux(cursor)(f(x)::res) // tail recursion
      }    
    }

    cursorMapAux(cursor)(Nil)
  }
  
  
  def printAll[T](c: EntityCursor[T]): Unit = cursorIter(c)(println(_))  
}