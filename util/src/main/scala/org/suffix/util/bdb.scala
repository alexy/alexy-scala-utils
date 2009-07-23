// Berkeley DB Java DPL storage for tweets
// http://blog.staale.org/2009/03/using-traits-to-configure-berkely-jdp.html
// http://forums.oracle.com/forums/thread.jspa?threadID=594269&tstart=15

package org.suffix.util.bdb

import com.sleepycat.je.{DatabaseException,Environment,EnvironmentConfig}
import com.sleepycat.persist.{EntityStore,StoreConfig}
import java.io.File

object Env {
    trait Transactional extends Env { config.setTransactional(true) }
    trait AllowCreate extends Env { config.setAllowCreate(true) }
}

class Env(val path:String) {
    val config = new EnvironmentConfig
    val e: Environment = new Environment(new File(path),config)
    def close: Unit = e.close

    def apply[T](f: Environment => T) = {
        try { f(e) } finally { e.close }
    }
}

object Store {
    trait Transactional extends Store { config.setTransactional(true) }
    trait AllowCreate extends Store { config.setAllowCreate(true) }
}

class Store(val env: Env, val name: String) {
    val config = new StoreConfig
    val s: EntityStore = new EntityStore(env.e,name,config)    
    def close: Unit = s.close
    
    def apply[T](f: EntityStore => T) = {
        // can also close the enclosing environment?
        try { f(s) } finally { s.close }
    }
}
