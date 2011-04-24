# Strings attached

Working with strings in Haskell is initially a great source of confusion. At first one learns that 

~~~ {.haskell}
    type String = [Char]
~~~

Aha! That's pretty elegant. `String` is just a type alias for a list of characters. Therefore all the familiar functions for a list work for a `String` too (e.g. [Data.List](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html)).

So far so good. 

The confusion starts when encountering an API which does not use a `String`, but a `ByteString`. Turns out, a plain `String` is not very efficient. It is a linked list internally and takes up more memory than an optimized structure. Also, some common operations are slow. Linked list has O(1) head and tail, but most of the operations are O(n), including getting a character at a specific index.

`ByteString` is an optimized `String`, internally a byte array. It provides a fast random access among other optimizations.

## The flavors of `ByteString`

`ByteString` is a rather more sophisticated type as a `String`. First of all, it supports various encodings. Note, qualified imports are often used with these. Many function names conflict with functions from `Prelude`.

~~~ {.haskell}
    import qualified Data.ByteString as B
~~~

This `ByteString` uses `Word8` arrays. It is good for binary IO.

~~~ {.haskell}
    import qualified Data.ByteString.Char8 as B
~~~

This `ByteString` uses Latin-1 `Char8` arrays. It is good for non-internationalized text.

~~~ {.haskell}
    import qualified Data.ByteString.UTF8 as B
~~~

This `ByteString` uses UTF8 encoded arrays. It is good for internationalized text.

Perhaps a bit more esoteric aspect of `ByteString` is, all those come as strict and lazy versions. Strict here means the `ByteString` is fully loaded into memory. Lazy `ByteString` loads the backing array elements to memory as needed, in buffered chunks.

~~~ {.haskell}
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as B
    import qualified Data.ByteString.Lazy.UTF8 as B
~~~

This has to do again with optimizations. Strict version is good for smaller strings. Random access is fast for arrays fully loaded into memory. Lazy version can represent huge strings without exceeding the memory limits.

`ByteString` can be converted to a byte array (binary IO) or `String` (text IO), and vice versa.

~~~ {.haskell}
    > :m Data.ByteString
    > :t pack
    pack :: [GHC.Word.Word8] -> ByteString
    > :t unpack
    unpack :: ByteString -> [GHC.Word.Word8]

    > :m Data.ByteString.Char8
    > :t pack
    pack :: String -> ByteString
    > :t unpack
    unpack :: ByteString -> [Char]
~~~

## OverloadedStrings 

GHC has an extension called `OverloadedStrings`. It enables a use of `String` literal syntax for custom types. `ByteString` library makes use of it too.

~~~ {.haskell}
    {-# LANGUAGE OverloadedStrings #-}

    import qualified Data.ByteString.Char8 as B

    someText :: B.ByteString
    someText = "I'm a ByteString"
~~~

Now the `String` literal is automatically converted to a `ByteString`, no need for an explicit `pack` function call. What's nice about `OverloadedStrings` extension is that it is available for any type having an instance of `IsString` type class. This is how `ByteString` does it:

~~~ {.haskell}
    instance IsString ByteString where
        fromString = pack
~~~

Let's finish this post with a small example which puts `OverloadedStrings` feature to work. `CasePreserving` is a type which stores a `String` in two versions, the original format and a lower case version.

~~~ {.haskell}
    {-# LANGUAGE OverloadedStrings #-}

    import GHC.Exts (IsString(..))
    import Data.Char

    data CasePreserving = CasePreserving { original :: String,
                                           lowerCase :: String }
                          deriving (Eq, Show)

    instance IsString CasePreserving where
        fromString s = CasePreserving s (map toLower s)

    caseTest :: CasePreserving
    caseTest = "Helo Wolrd"
~~~

Load the file into REPL to test it.

~~~ {.haskell}
    *Main> :l Test.hs
    *Main> caseTest 
    CasePreserving {original = "Helo Wolrd", lowerCase = "helo wolrd"}
~~~
