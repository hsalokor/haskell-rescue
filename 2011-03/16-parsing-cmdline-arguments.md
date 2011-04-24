# Parsing command-line arguments with Haskell

I'm writing this because I found yet another great thing about Haskell: it's actually a nice language for writing command-line tools! A command-line tool written in Haskell reads like the manual for the tool. Pattern matching makes it easier and cleaner than in any other language (prove me wrong).

You can match exact strings for commands like ``"init"``, ``"update"``, ``"status"``, as well as any number of following arguments that you can use in the implementation of that command.

~~~ {.haskell}
    main = getArgs >>= rebass

    rebass ["init", name] = do
     -- init using given name, ignore rest of arguments   
    rebass ["update"] = do
     -- update, ignore arguments
    rebass ["status"] = do
         -- show status, ignore arguments
    rebass _ = do
     -- show usage as none of the patterns matched
~~~

Here's what I'm working on btw: <https://github.com/raimohanska/rebass>

