# Reading JSON data, the quick and dirty way

Reading JSON data in Haskell "by the book" requires lot of fiddling around, and resulting code is typically not very readable. The performance is good, but the effort required to read the data may seem intimidating.  

Usually you just need to read some data (performace be damned), then there's a nice little trick you can use to get started.

## Setting up

First, install JSON parser module called `Data.Aeson` (at this writing, 0.3.2.4) using cabal.

~~~ {.bash}
    cabal install aeson
~~~

The module Text.JSON has a submodule called Text.JSON.Generic. This module supports a form of introspection, which allows you to read data to existing data record with minimal fuzz.

The trick requires GHC `DeriveDataTypeable` language extension (we use `OverloadedStrings` for convenient automatic string conversions), and thus the file must begin with following compiler pragma:

~~~ {.haskell}
    {-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
~~~

We'll also need `Data.Data` and `Data.Typeable` type classes:

~~~ {.haskell}
    import           Data.Typeable
    import           Data.Data
~~~

## Testdata

TODO, TODO

~~~ {.JSON}

    [
        {
            "title": "First blog post",
            "text": "Lorem ipsum dolor omet"
        },

        {
            "title": "Second blog post",
            "published": false,
            "text": "Again, lorem ipsum dolor omet"
        },

        {
            "title": "Heated debate",
            "published": true,
            "text": "Again, lorem ipsum dolor omet",
            "comments": [
                {
                    "author": "Anonymous",
                    "comment": "This sucks"
                },

                {
                    "author": "Mr. Smith",
                    "email": "mr.smith@example.tld",
                    "comment": "No, it doesn't"
                }
            ]
        }
    ]
~~~ 

## TODO, TODO, TODO...

~~~ {.haskell}

    {-# LANGUAGE DeriveDataTypeable #-}
    module Main where

    import System.Environment(getArgs)

    import Text.JSON()
    import Text.JSON.Generic
    import Data.Typeable
    import Data.Data

    data BlogComment = BlogComment {
            author  :: String,
            email   :: String,
            comment :: String
        }
        deriving(Eq, Show, Data, Typeable)

    data BlogEntry = BlogEntry {
            title     :: String,
            published :: Bool,
            text      :: String,
            comments  :: [BlogComment]
        }
        deriving(Eq, Show, Data, Typeable)

    load :: String -> [BlogEntry]
    load d = decodeJSON d

    main :: IO ()
    main = do
        args <- getArgs
        d <- readFile $ head args
        let entries = load d
        putStr $ encodeJSON entries
~~~
