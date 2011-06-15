# Rx for Haskell, or my adventure in the land of typeclasses

A while a ago I hacked quite a lot on Reactive Extensions for Javascript, which resulted in writing a simple game [Worzone](http://juhajasatu.com/worzone) and posting a bunch of blog posts. Lately I've been more interested in learning Haskell.

A few days ago I decided to try to write a version of [Rx in Haskell](http://github.com/raimohanska/rx-haskell), and that's what I'm going to tell you about in this post. In the following, I expect you to understand some Haskell and to know the basic concepts of Reactive Extensions.  

Briefly, Rx is a framework for reactive programming. The main concept there if Observable, which is a source of events that you can subscribe to. The thing that makes Rx interesting is the concept of Combinators. For instance, in Rx for Javascript, you can create Observables for click events of HTML buttons. In the following example (Thanks, Joni) these clicks are first mapped in to integers +1 and -1 using the `Select` combinator, then merged into a single `Observable` using `Merge`, then converted into a sum using `Scan`. Finally, a side-effect is added using `Subscribe`: a label is updated so that it will display a counter that can be increased and decreased by pressing the + and - buttons. 

~~~ {.javascript}
$(function() {
  function always(x) { return function(_) { return x }}

  var incr = $('#incr').toObservable('click').Select(always(1))
  var decr = $('#decr').toObservable('click').Select(always(-1))

  incr.Merge(decr)
    .Scan(0, function(total, x) { return total + x })
    .Subscribe(updateCount)

  function updateCount(total) {
    $('#count').html(total)
  }
})
~~~

Please read some of Matt Podwysocki's postings on Rx, such as [this one](http://codebetter.com/matthewpodwysocki/2010/02/23/introduction-to-the-reactive-extensions-for-javascript-creating-observables/). I've also written some rants about Rx on my blog, such as [this one](http://nullzzz.blogspot.com/2011/02/game-programming-with-rx-js.html)

## Rx with typeclasses

So, with my background in OOP, I began by declaring a typeclass for
`Observable`. Like this:

~~~ {.haskell}
class Observable a observable where
  subscribe :: observable -> Observer a -> IO Disposable

type Observer a = (a -> IO ())
type Disposable = IO ()
type Subscribe a = (Observer a -> IO Disposable)
~~~

I had the idea that with the `Observable` typeclass it should be easy to make your own `Observables` just by declaring an instance for them. So, I wrote an instance for `List`:

~~~ {.haskell}
instance Observable a ([a]) where
  subscribe list observer = do
    mapM observer list
    return (return ())
~~~

And in GHCI:

~~~ {.haskell}
Rx> subscribe ["a", "b"] putStrLn
a
b
~~~

So, I can subscribe an `Observer` (here the  `putStrLn` function) to an array. Promising start. Note that I needed to turn on some GHC flags to allow stuff like multiparameter typeclasses and making instances for arrays. So, in the beginning of my module, I had this:

~~~ {.haskell}
{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances #-}
module Rx where
import Control.Monad
~~~

Next, let's write some combinators.. Like

~~~ {.haskell}
select :: Observable a a' => (a -> b) -> a' -> Subscribe b 
select func observable = (\ observerB -> subscribe observable (convert observerB))
  where convert observerB = observerB . func
~~~

This is supposed to convert an `Observable a` into an `Observable b` using a given mapping function. Should work, right? However,

~~~ {.haskell}
*Combinators> subscribe (select show [1, 2, 3]) putStrLn
<interactive>:1:11:
    No instance for (Observable a [t])
      arising from a use of `select' at <interactive>:1:11-31
    Possible fix: add an instance declaration for (Observable a [t])
    In the first argument of `subscribe', namely
        `(select show [1, 2, 3])'
    In the expression: subscribe (select show [1, 2, 3]) putStrLn
    In the definition of `it':
        it = subscribe (select show [1, 2, 3]) putStrLn
~~~

This is where I got stuck for quite a while. It should be just fine, I reasoned: 

- `[1, 2, 3]` has an instance for `Observable Int`
- `(select show [1, 2, 3])` should be `Observable String`, because show maps `Int` to `String`
Still, it does not compile. It took quite a while to figure out what's wrong. See:

~~~ {.haskell}
*Combinators> let selectSpecific = select :: (Int -> String) -> [Int] -> Subscribe String
*Combinators> subscribe (selectSpecific show [1, 2, 3]) putStrLn
1
2
3
~~~

## Enter simplistic approach

It seems that my example compiles and runs if I just add some type annotations. For me this seems like the type inference system in Haskell (GHC) is lacking. It's of course more probable that I just don't get it :) Anyways, having to annotate the code using this "API" is unacceptable, so I decided to try to do this without the typeclass:

~~~ {.haskell}
type Observer a = (a -> IO ())
type Disposable = IO ()
type Subscribe a = (Observer a -> IO Disposable)

observableList :: [a] -> Subscribe a
observableList list observer = do
    mapM observer list 
    return (return ())
~~~

And then some combinators:

~~~ {.haskell}
select :: (a -> b) -> (Subscribe a) -> Observer b -> IO Disposable
select convert subscribe observer = subscribe (observer . convert)

filter :: (a -> Bool) -> (Subscribe a) -> Observer a -> IO Disposable
filter predicate subscribe observer = subscribe filteredObserver
  where filteredObserver a = if (predicate a) then (observer a) else return ()
~~~

For me, this seems a bit less elegant, as I have to explicitly say `observableList` to be able to subscribe to a list. But the upside is that it works and does not require type annotations in the client code:

~~~ {.haskell}
*Combinators> select show (Combinators.filter even $ observableList [1, 2]) putStrLn
2
~~~

## Monads Gonads

Now that's convenient. But, as Joni Freeman has convinced me,  `Observables` are actually  `Monads` and  `Functors` too, so I wanted to make an instance for each. Like

~~~ {.haskell}
instance Functor Observable where
  fmap = select
~~~

But alas again, won't compile. I twiddled around quite a while again only to come into the conclusion that I cannot make the `Observer` (which is just a subscribe function really) an instance of `Monad`,  `Functor` or anything. So, I had to convert it into a `data`:

~~~ {.haskell}
data Observable a = Observable {subscribe :: Subscribe a}
type Observer a = (a -> IO ())
type Subscribe a = (Observer a -> IO Disposable)
type Disposable = IO ()
~~~

Then, having refactored my select and filter combinators to operate on this new definition of  `Observable` and writing the  `selectMany` combinator, I'm finally able to proudly declare:

~~~ {.haskell}
instance Functor Observable where
  fmap = select

instance Monad Observable where
  return a = observableList [a]
  (>>=) = selectMany
~~~

Good shit! I don't have a very good idea what I'll gain by `Monads` and
 `Functors`, but I'm going to find out. To get back to the original
one-liner for printing list items into the console, here's how it's done
with the "final" solution:

~~~ {.haskell}
*Rx> subscribe (observableList ["a", "b"]) putStrLn
1
2
3
~~~

And, here's an example with the `select` and `filter` combinators:

~~~ {.haskell}
*Rx> subscribe (select show (Rx.filter even $ observableList [1, 2])) putStrLn
2
~~~

Pls find the source code in [Github](http://github.com/raimohanska/rx-haskell).
