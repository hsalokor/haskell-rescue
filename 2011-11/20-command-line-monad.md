The Command Prompt Monad
========================

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

Here [String] means a list of strings and IO means that the function is not pure: 
it performs some IO, and hence is not necessarily deterministic and may have side-effects.

Transformed to Haskell, you could have something like this:

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

I would say that the `>>=` operator is the equivalent of the pipe for Haskell IO!

Now, suppose we wanted to implement the funcions above.

TODO

In `ghci` we can have a look at the signature of this operator:

~~~ .haskell
Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
~~~

That means that the operator (actually a function) is applicable to any Monad `m`  and for any types `a` and `b`.