{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import System.Directory
import Data.List
import Lib



filterCSVFiles :: [FilePath] -> [FilePath]
filterCSVFiles file = filter (isInfixOf ".csv") file

main :: IO ()
main =
  do
    directory <- fmap filterCSVFiles $ listDirectory "."
    files <- mapM (TIO.readFile) directory
    let fileLines = map (T.splitOn "\r") files
    let results = map (convert) fileLines
    let convertedFiles = zip directory results
    mapM_ (\(fileName, json) -> TIO.writeFile (T.unpack (T.replace (T.pack ".csv") (T.pack ".json") (T.pack fileName))) (json)) (convertedFiles) -- TODO: This is too confusing, but it will do.
    return ()
