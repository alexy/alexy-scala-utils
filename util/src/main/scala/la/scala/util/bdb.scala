// Berkeley DB Java DPL storage for tweets
// http://blog.staale.org/2009/03/using-traits-to-configure-berkely-jdp.html
package la.scala.util.bdb

class Env(val path:File) {
    trait Transactional extends Env { config.setTransactional(true) }
    trait AllowCreate extends Env { config.setAllowCreate(true) }
 
    val config = new EnvironmentConfig()
    def apply[T](f:(Environment) => T) = {
        val env = new Environment(path, config)
        try { f(env) } finally { env.close() }
 
    }
}
