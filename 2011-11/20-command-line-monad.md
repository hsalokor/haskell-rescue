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
cat lolcats.txt | grep cat | xargs touch
~~~

I should say that

"cat lolcats.txt" :: IO [String]
"grep cat"        :: [String] -> IO [String]
"xargs touch"     :: [String] -> IO ()

Here [String] means a list of strings and IO means that the function is not pure: 
it performs some IO, and hence is not necessarily deterministic and may have side-effects.

In Haskell, you could write this like here:

~~~ .haskell

Prelude System.Directory Data.List> readFile "lolcats.txt" >>= return . filter (isInfixOf "cat") . lines >>= (flip writeFile) ""

~~~ 

TODO
