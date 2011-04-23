# Cooking delicious fish

Many Haskell newcomers stumble when presented with wonderful but abstract utility functions from the stdlib. This is mostly because Haskell documentation lacks easy to understand usage examples. In this short entry I will introduce one such nice function and an example how to use it.

Like a good Haskell citizen, let's start with its type signature:

~~~ {.haskell}
    (a -> m b) -> (b -> m c) -> (a -> m c)
~~~

So, those are the ingredients that go along with a fish ``>=>``, the name of the function[^1]. Operators (functions with symbolic names) in Haskell are used in an infix form:

~~~ {.haskell}
    x1 >=> x2
~~~

Here ``x1`` has a type ``(a -> m b)`` and ``x2`` has a type ``(b -> m c)``. The type of ``x1 >=> x2`` is therefore ``(a -> m c)``. Now let's put all that aside for awhile and approach this example from a different angle.

## Parse, convert and validate

A common programming task in a networked world is to parse some input data coming from a user. It needs to be parsed, converted to a proper type and often validated too.

~~~ {.haskell}
    parseAge :: String -> Maybe Int
    parseAge s = case reads s of   
        [(age, "")] -> Just age   
        _           -> Nothing
    
    maxVal :: Int -> Int -> Maybe Int
    maxVal x y | y > x = Nothing
               | otherwise = Just y
    
    minVal :: Int -> Int -> Maybe Int
    minVal x y | y < x = Nothing
               | otherwise = Just y
~~~

I'm using a Maybe type here since it is the most simple type which can represent failed computations. It is easy to replace that with a more sophisticated type, one capturing the failure reasons and other stuff but to keep the example short we will use a simpler type now. Anyway, given a set of such utility functions we would like to compose more complex functionality. This is where our fish excels. 

~~~ {.haskell}
    parseAge >=> (minVal 18) >=> (maxVal 24)
~~~

The above expression has a type ``(String -> Maybe Int)`` and it works as expected. We'll get ``Just x`` if age can be parsed as an integer, and it is at least 18 but at most 24. Otherwise we'll get ``Nothing``. 

~~~ {.haskell}
    > :m +Control.Monad
    > let pAge = parseAge >=> (minVal 18) >=> (maxVal 24)
    > pAge "20"
    Just 20
    > pAge "20a"
    Nothing
    > pAge "17"
    Nothing
    > pAge "25"
    Nothing
~~~

## Conclusion

Functions are often composed in Haskell with ``.`` function. However, that simple composition function won't work if the functions composed are monadic functions ``(a -> m b)``. Monadic functions can be composed with ``>=>`` function.

[^1]: The real name of the function is Kleisli composition, but the operator symbol selected for that function looks a lot like a fish.

