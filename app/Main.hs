{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Directory
import Data.List
import System.Console.CmdArgs
import System.Process
import qualified Data.Text.Lazy as LazyT
import Data.Aeson.Text
import Data.Aeson
import GHC.Generics
import Lib

-- filterCSVFiles :: [FilePath] -> [FilePath]
-- filterCSVFiles file = filter (isInfixOf ".csv") file
--
-- main :: IO ()
-- main =
--   do
--     directory <- fmap filterCSVFiles $ listDirectory "."
--     files <- mapM (TIO.readFile) directory
--     let fileLines = map (T.splitOn "\r") files
--     let results = map (convert) fileLines
--     let convertedFiles = zip directory results
--     mapM_ (\(fileName, json) -> TIO.writeFile (T.unpack (T.replace (T.pack ".csv") (T.pack ".json") (T.pack fileName))) (json)) (convertedFiles) -- TODO: This is too confusing, but it will do.
--     return ()


data Options = Options {path :: String}
              deriving (Show, Data, Typeable)

data File = File {
  filename :: T.Text,
  genre :: T.Text,
  filepath :: T.Text
}
  deriving (Generic, Show)

instance ToJSON File where
  toEncoding = genericToEncoding defaultOptions

splitter :: T.Text -> [T.Text]
splitter n | T.any (== '\r') n = T.splitOn "\r" n
           | T.any (== '\n') n = T.splitOn "\n" n

createFile :: FilePath -> File
createFile f = File {filename = fname, genre = fgenre, filepath = fpath}
  where
    fgenre =  case length directories >= 3 of
               True -> head $ tail $ tail $ reverse $ directories
               _ -> "Unknown"
    fname = T.replace ".json" "" $ head $ reverse $ directories
    directories = T.splitOn "/" fpath
    fpath = (T.replace ".csv" ".json" (T.pack f))

options = Options{path = def &= help "Path to directory" &= typ ("/Songs" :: String)} &= summary ("csvtojson, v1") &= program "csvtojson"

getPath :: Main.Options -> String
getPath s = path s

process :: Main.Options -> IO ()
process Options{path = ""} = TIO.putStrLn "--path argument required."
process args = do
  let directory = (getPath args)
  csvFilePaths <- readCreateProcess (shell ("find " ++ directory ++ " -name \"*.csv\"")) ""
  let files =  map (createFile) (lines $ csvFilePaths)
  let fileInfo = map (T.unpack . (T.replace ".json" ".csv") . filepath) $ files
  csvFiles <- mapM (TIO.readFile) $ fileInfo
  let results = map (convert . splitter) $ csvFiles
  let convertedFiles = filter (\(_, results) -> results /= (LazyT.toStrict $ encodeToLazyText emptyProfile)) $ zip files results
  mapM_ (\(info, result) -> TIO.writeFile (T.unpack (filepath info)) result) convertedFiles
  let metadata = LazyT.toStrict $ encodeToLazyText (map fst convertedFiles)
  TIO.writeFile (directory ++ "/metadata.json") metadata

main = do
  arguments <- cmdArgs options
  process arguments
  return ()
