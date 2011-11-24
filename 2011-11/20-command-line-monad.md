The Bash Monad
==============

You ar probably aware of using Unix command-line tools and chaining them together using the pipe (|). 
This is very much similar to how monadic Haskell programs work: you chain a bunch of IO actions together using the `>>=` symbol.

When writing this kind of chains, you are usually interested in these aspects of each of the tools in the chain:

- Input
- Output
- Side-effects

Usually only the last tool in the chain will actually perform any side-effects; 
the others just read files and process input text, producing some output text.

Let's now try to define the "type signature" of these commands.

So, suppose you have a command-line like

~~~ .bash
cat lolcats.txt | xargs touch
~~~

I should say that

"cat lolcats.txt" :: IO [String]

"xargs touch"     :: [String] -> IO ()

I'm using the Haskell type signature notation here. There [String] means a list of strings and IO means that the function is not pure: 
it performs some IO, and hence is not necessarily deterministic and may have side-effects. 
The `xargs touch` part has a signature `IO ()` (pronounces IO unit) that indicates that it consumes a list of strings and performs an IO action
that doesn't return any value.

If you wrote this as a Haskell program, you could have functions like this:

~~~ .haskell
readLolCats :: IO [String]
writeCatFiles :: [String] -> IO ()
~~~ 

Let's just assume that these functions were already implemented and see how we can chain them together.
Well, that would be using the `>>=` operator. 

So the whole thing would go like 

~~~ .haskell
readLolCats >>= writeCatFiles
~~~

I would say that the `>>=` operator is the equivalent of the shell pipe for Haskell IO!

The functions above could be implemente like

~~~ .haskell
grepCats = return . filter (isInfixOf "cat")
writeCatFiles =  mapM_ (flip writeFile $ "")
~~~

Say what? The first function uses `.` to compose two functions. What are their types? Let's ask GHCI:

~~~ .haskell
*CommandPrompt> :t return
return :: Monad m => a -> m a
*CommandPrompt> :t filter (isInfixOf "cat")
filter (isInfixOf "cat") :: [[Char]] -> [[Char]]
~~~

So, return seems to be related to something called Monads. 
Buy hey, IO is a Monad, so practically `return` takes any value a and returns IO a.
Thus is allows you to inject a value "into IO". Or any monad. Let's say IO though.

The latter part says that it filters a list of Strings, keeping only those that include "cat". 
The type signature [[Char]] -> [[Char]] is equivalent to [String] -> [String].

When we compose these functions using `.`, we get a function having a type signature like

~~~ .haskell
*CommandPrompt> :t grepCats
grepCats :: [String] -> IO [String]
~~~

We knew that though, right.

How does `writeCatFiles` work, then? Well, it uses mapM_, which is Haskell's equivalent to forEach in some languages.

TODO

.......... 



In `ghci` we can have a look at the signature of this operator:

~~~ .haskell
Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
~~~

That means that the operator (actually a function) is applicable to any Monad `m`  and for any types `a` and `b`.