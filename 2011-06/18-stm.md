# A Luxury Vehicle Called The STM Monad

A couple of days ago I didn't know much about how you handle mutable state in concurrent Haskell programs. Then I thought about STM (Software Transactional Memory) the idea of which is that you can do your business with your mutable state in a transactional way. 

The classic toy example is the Bank that holds its account balances in the memory of a computer. It wants to make sure that when dollars are transferred from account to account, i.e. money is taken from an account and added to another, this happens as a complete transaction, or does not happen at all. Please read Simon Peyton Jones's paper on the subject: [Beautiful concurrency](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf). 

Anyways, you could solve the bank problem using Locks, but they are
quite prone to deadlocks. In the STM model, there are no locks, but instead it "optimistically" tries to do your stuff and if a conflict with another thread occurs, it does a rollback and tries again. Nice? Here's how you could do it in Haskell: 

~~~ {.haskell}

{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Exception
import Data.Typeable

data Account = Account { balance :: TVar Int }
data NotEnoughBalance = NotEnoughBalance String
   deriving (Show, Typeable)

instance Exception NotEnoughBalance

withdraw :: Account -> Int -> STM ()
withdraw (Account balance) amount = do
  bal <- readTVar balance
  let newBal = bal - amount
  when (newBal < 0) $ throw $ NotEnoughBalance "not enough money"
  writeTVar balance newBal

deposit account amount = withdraw account (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = atomically $ do 
  deposit to amount
  withdraw from amount
~~~

As you see, I've even included balance checking: the withdraw function
throws an exception if there's not enough money.

But hey! What if I do something irreversible in my transactional code?
Now the system would not be able to rollback correctly! 

That was my first (ok, second) thought when I read about how STM works.
But Haskell solves this problem in the most elegant way: by using the
STM Monad. When you're in this Monad, you cannot have irreversible
side-effects. Just try adding a `putStrLn` into the `withdraw` function
and you'll see it won't compile. As you see, in the above example, the former
of the `do` blocks is in the STM monad while the latter is in the IO
monad.

Monads kick ass!

And no, I'm not writing an online banking system. But, I'd
certainly use Haskell if I was.
