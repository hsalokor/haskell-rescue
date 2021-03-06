# Object-Oriented Haskell

I wanted to get back to the subject of how a coder with OO background
can pick on Haskell. Haskell is not an object-oriented language,
but surely has facilities for doing similar things. I'm in the process
of discovering these facilities and also learning to think more in terms
of functions than in terms of objects. This blog post will start with
something that I learned and end with some open questions, so don't
expect a cookbook-style entry this time..

Anyways, in OO, separating interface from implementation is a key thing 
in managing complexity. Therefore we use interfaces, packages, private members etc. 
You know all this, right. Also, you might know that none of the above
facilities exist in Haskell. But, let's start with an example now. And let's pretend 
we are in a desperate need of a new logging framework. In Java, we could start with

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
methods and an agreement that all logger implementations must have them. The
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

The Haskell version isn't so different:

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

In the above example, the closure, or the hidden part of the "object" actually consists of the statically defined method `log`. In the next example, there's also a constructor parameter involved:

~~~ {.haskell}

import Control.Monad(when)

data Logger = Logger { info :: Log, warn :: Log, error :: Log }
data Level = Info|Warn|Error deriving (Show, Ord, Eq)
type Log = String -> IO ()

sysOutLogger threshold = Logger (log Info) (log Warn) (log Error)
  where log lvl msg = when (lvl >= threshold) $ putStrLn $ show lvl ++ " - " ++ msg

~~~

With this logger, you can specify the logging threshold in the
constructor. See:

~~~ {.haskell}
*Main> let logger = sysOutLogger Warn
*Main> info logger "lol"
*Main> warn logger "wtf?"
Warn - wtf?
~~~

Dead simple. Now bash me for trying to bring my OO thinking to the
functional wonderland. Then bash me some more, because the rest of this posting
contains more questions than answers. Then tell me how I deploy this kind of a logging
framework in Haskell. I mean, how do I pragmatically obtain the Logger
in all of my IO code. With Java I'm used to 

~~~ {.java}

static Logger logger =
Logger.getLogger(DaoVisitorFactoryProviderWrapperContainer.class);

~~~

Which implies that the logging framework is statically configured and
is practically using global varibles, which is a sin, but maybe
acceptable in the logging example. Now how do I do similar stuff in Haskell? Should I resort to
global variables there too? I would feel a bit sad passing around a
Logger to every possible service that might need it. 

In some other cases I might use a
Dependency Injection framework to get implementations of certain
services.
 
Like, if I've got an Application that's using a Web Service in the
Internet for getting stock quotes and then displaying them to the user
as stunning 3D graphs. When running in a test setup, I'd like to use a
different URL for the Web Service. I'd also like to be able to configure
the app so that I can run it locally on my machine, so that the stock
quotes would actually be generated randomly instead of using a Web
Service at all. Like

~~~ .haskell

data Quote = Quote { name :: String, value :: Int } deriving (Show)
data QuoteService = QuoteService { fetch :: IO [Quote] }

realQuoteService url = QuoteService undefined

fakeQuoteService = QuoteService $ return [ Quote "NOK" 0 ]

~~~

Now putting together the application would involve
creating a bunch of services and stuff and the passing them around to,
like, the GUI layer or so. I could create the services differently
depending on whether we are running in production, test, or local setup.
Now is this just the Enterprise Java Architect mindset or do we need
something like Dependency Injection in a larger scale application? Will
we end up in a mess without something like it? In Java we did, because
we had to manually pass tons of arguments to put the shit together.
Using DI, the art of binding interfaces to implementations is separated
from actual object creation. Using DI magic, I don't need to pass stuff down the chain of constructors. 

Any experiences with Enterprise Haskell anyone? :)
