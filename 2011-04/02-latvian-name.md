# Haskell Knows Your Latvian Name

My colleague presented a devilishly clever, yet simple algorithm for generating a Latvian version of your name. Here's the implementation in Haskell:

~~~ {.haskell}
    import System.Environment(getArgs)
    main = do
        args <- getArgs
        putStrLn $ latvian args
      where
          latvian = unwords . map (++ "s")    
~~~

Copy this code to a file, say latvian.hs then

~~~ {.haskell}
    runhaskell latvian.hs <your name>
~~~

And you'll know what you'd be called in case you were lucky enough to be born in Latvia.'
