I call monads like IO and STM "action monads", because each monadic
value, such as `getName :: IO String` is actually an action that will 
yield the value when executed. In this posting I'll throw you a
challenge: how would you implement the `State` monad, as described
below.

Generally, a Monad can be constructed as below.
 
1. Introduce your data type like `data State`
2. Introduce the typeclass instance `instance Monad State`
3. Implement the rest of the fucking Monad

Obviously the hard part is the third one. But now..

The State Monad Challenge
=========================

The challenge is to implement the `State` monad, which will allow you to
write algorithms with the benefits of mutable state without having
actual side-effects or mutable state. Um? Ok, let's say it allows you to
read and write into a single variable, while actually carrying the
current state in the stack. The function that use `State` don't have to
worry about this though. You should be able to write stuff like

~~~ .haskell
sum :: Num a => [a] -> a
sum nums = runState 0 $ do forM_ nums $ \num -> do 
                                acc <- readState
                                writeState (acc + num)
                           readState
~~~

That's a sum algorithm using state. Not a very smart one, but serves the
purpose of showing what you can do with `State`. It's important to note
that the signature of `sum` indicates that it's pure, even though the
implementation uses `State` internally.

We can refactor it to two pieces to separate the pure from the stateful:

~~~ .haskell

sum nums = runState 0 $ add nums >> readState
add :: Num t => [s] -> State s ()
add nums = forM_ nums $ \num -> do acc <- readState
                                   writeState (acc + num)
~~~

From here we can imply a few things:

1. State has two type parameters. Let's call them s (for state) and a
   (for return value)
2. There are functions `readState` and `writeState` for reading and
   writing to the state variable
3. There's a function `runState` for running a calculation with a given
   start state.

So let's start with

~~~ .haskell
{-# LANGUAGE MultiParamTypeClasses #-}
module State(State, runState, readState, writeState) where

data State s a

instance Monad (State s) where
  return x = undefined
  action1 >>= action2 = undefined

runState :: s -> State s a -> a
runState st0 action = undefined

readState :: State s s
readState = undefined

writeState :: s -> State s ()
writeState x = undefined
~~~

Wow wow wow?! Lot's of stuff here? Well, that's just the interface for
the State Monad. As you can see, I've allowed MultiParamTypeClasses
there and declared that `State s` is a Monad. The tricky parts are
coming up next. You should decide

1. What kind of data the "monadic value" of State should encapsulate,
   i.e. what is the data constructor of State
2. How will you implement the monad methods `return` and `>>=`
3. How will you then implement `runState`, `writeState` and `readState`.

As a tip, I suggest you start by considering what a single "state
action" must be able to do, which is something like "given a starting
state, it will return an end state and a return value"..
