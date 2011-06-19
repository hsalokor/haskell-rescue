# Object-Oriented Haskell

I wanted to get back to the subject of how a coder with OO background
can product Haskell code. Haskell is not an object-oriented language,
but surely has facilities for doing similar things. Separating interface
from implementation is a must. I'm running a bit short on good examples
now, but let's pretend we are in a desperate need of a new logging
framework. In Java, we could start with

~~~ {.java}
interface Logger {
  void info(String msg);
  void warn(String msg);
  void error(String msg);
}
~~~

And POW, we have separated interface from implementation and are ready
to start writing the EJBLogger class. 

In Javascript, there are no
interfaces, but you can call any method with any arguments on any
object. So you may agree that the logger API consists of these three
methods and that all logger implementations must have them. The
"interface" is actually the API documentation.

But, in Haskell? We've got

- functions
- `types` (which are just synonyms for other types)
- `datas` which are like immutable structs to simplify
- `typeclasses` which are somewhat similar to OO interfaces. You can
  declare an `instance` for a `data` to give it this interface.

Typeclasses may seem like the obvious solution for Logger, for instance,
but they are not as practical as you'd think at first. That's what I
discovered when trying to model Rx for Haskell, as I already told you
about in my [previous post](http://haskellrescue.blogspot.com/2011/06/rx-for-haskell-my-first-monad.html).

I found out that the less sexy `data` is surprisingly the most practical choice for OO-like constructs. With that, you can the same thing as you've probably done with Javascript: Your constructor function implicitly creates a closure that stores the hidden internal state of your object and returns a `data` exposes the object's public methods. In Javascript you could say something like

~~~ {.javascript}
var ConsoleLogger = function() {
  function log(prefix, message) {
    console.log(prefix + " - " + message)
  }
  return {
    info : function(msg) { log("INFO", msg }
    warn : function(msg) { log("WARN", msg }
    error : function(msg) { log("ERROR", msg }
  }
} 
~~~

Got it? The Haskell version isn't so different:

~~~ {.haskell}
data Logger = Logger { info :: Log, warn :: Log, error :: Log }
type Log = String -> IO ()
sysOutLogger = Logger (log "Info") (log "Warn") (log "Error")
  where log level msg = putStrLn $ level ++ " - " ++ msg
~~~

Except for, of course, that it's type-safe and has a real interface for
Logger. It also appears a bit shorter, because of Haskell's more compact
formatting conventions and its support for partially applied functions
(currying).
