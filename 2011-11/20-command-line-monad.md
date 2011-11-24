The Bash Monad
==============

You've probably used some Unix command-line tools and chained them together using the pipe (|). 
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

~~~ .haskell
"cat lolcats.txt" :: IO [String]
"xargs touch"     :: [String] -> IO ()
~~~

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

I'd like to say that the `>>=` operator is the equivalent of the shell pipe for Haskell IO!

The functions above could be implemented like

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

So, `return` seems to be related to something called Monads. 
Buy hey, IO is a Monad, so practically `return` takes any value a and returns IO a.
Thus is allows you to inject a value "into IO". Or any monad. Let's stick to IO for now, though.

The latter part says that it filters a list of Strings, keeping only those that include "cat". 
The type signature `[[Char]] -> [[Char]]` is equivalent to `[String] -> [String]` (Strings are just lists of Chars).

When we compose these functions using `.`, we get a function having a type signature like

~~~ .haskell
*CommandPrompt> :t grepCats
grepCats :: [String] -> IO [String]
~~~

We knew that though, right.

How does `writeCatFiles` work, then? Well, it uses mapM_, which is Haskell's equivalent to forEach in some languages: 
it does something with each of the elements in the given list.

~~~ .haskell
*CommandPrompt> :t mapM_
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
~~~

As you can see, it takes a function that is applicable to the elements in the given list of values of type `a`. 
We are using it in the IO monad, so practially mapM_ applies the given function `a -> IO b` to all elements. 
The return type b doesn't matter; mapM_ discards the possible return value. Hence, your function may very well be
of type `a -> IO ()`. 

In our `writeCatFiles` function, we provide mapM_ with `(flip writeFile $ "")` which has the
type `FilePath -> IO()`. Now FilePath is actually a typeclass and there's a String-instance for it, so String can
be used instead of FilePath. So why `flip`? That's because we need to be able to feed file names to this function.
It just happens to be that `writeFile` has file name as its first parameter and file contents as the second.
With `flip` we can reverse the ordering of the parameters, and by currying the flipped function with "", we get 
a function that accepts a filename and writes the empty string into that file.

Currying? Well, suppose you've got a function with two params, like

~~~ .haskell
*CommandPrompt> :t flip writeFile
flip writeFile :: String -> FilePath -> IO ()
~~~

Here the function `(flip writeFile)` accepts first file contents and then the file name. 
Now if we apply one parameter, we get

~~~ .haskell
*CommandPrompt> :t (flip writeFile) $ ""
(flip writeFile) "" :: FilePath -> IO ()
~~~

.. which is acceptable for our little "for-loop" : we can use this function as the first argument for mapM_ and get
the empty string written to all files given in the input list.

So, currying is just applying a parameter to a function, and getting another function with one less param.

Now what exactly is a Monad?
============================

One way to put it is that its a type for actions that can be chained using the >>= operator. 
Also, a Monad needs to have the `return` function defined, for injecting values into the monad.

So in terms of `bash`, you can say that the pipe ´|´ is the equivalent of `>>=`.

Now, what would be the bash-equivalent of `return`? A free T-shirt for the first correct answer!