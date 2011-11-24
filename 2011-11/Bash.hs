module CommandPrompt where

import System.Directory
import Data.List

readLolCats :: IO [String]
readLolCats = readFile "lolcats.txt" >>= return . lines

grepCats :: [String] -> IO [String]
grepCats = return . filter (isInfixOf "cat")

writeCatFiles :: [String] -> IO ()
writeCatFiles =  mapM_ (flip writeFile $ "")

prompt = readLolCats >>= grepCats >>= writeCatFiles
