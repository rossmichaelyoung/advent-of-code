module Utils.Helper where

import System.Environment

getArgAndReadFile :: IO String
getArgAndReadFile = fmap head getArgs >>= readFile