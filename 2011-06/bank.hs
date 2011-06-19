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
