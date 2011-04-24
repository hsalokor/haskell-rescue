{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Environment(getArgs)

import           Text.JSON()
import           Text.JSON.Generic
import           Data.Typeable
import           Data.Data
import           System.IO.Error

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

load :: String -> IO (Either IOError [BlogEntry])
load d = do
            result <- try $ decodeJSON d
            return $ result

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    entries <- load input 
    case entries of
        Left err      -> putStr "Failed to read JSON data: "
        Right entries -> putStr $ show entries

