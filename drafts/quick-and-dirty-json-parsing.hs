{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Environment(getArgs)

import           Data.Aeson.Types hiding (fromJSON)
import           Data.Aeson.Generic
import           Data.Typeable
import           Data.Data
import           Data.Text hiding (head)
import qualified Data.Text.IO as T

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

parseEntries :: Text -> Result [BlogEntry]
parseEntries input = fromJSON (String input)

main :: IO ()
main = do
    args <- getArgs
    input <- T.readFile $ head args
    case parseEntries input of
        Error err       -> putStr $ "Failed to read JSON data: " ++ show err
        Success entries -> putStr $ show entries
    return ()

