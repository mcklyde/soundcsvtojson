{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import Data.Aeson.Text
import Data.Aeson
import GHC.Generics
import Text.Regex

data SoundFile = SoundFile {
  name :: T.Text,
  title :: T.Text,
  tempo :: T.Text,
  notes :: [Note],
  lyrics :: [T.Text],
  video :: T.Text,
  score :: T.Text,
  key :: T.Text,
  timeSignature :: T.Text
} deriving (Generic, Show)

data Note = Note {
  notename :: T.Text,
  octave :: Int,
  timing :: T.Text
} deriving (Generic, Show)

instance ToJSON Note where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SoundFile where
    toEncoding = genericToEncoding defaultOptions

emptyProfile :: SoundFile
emptyProfile = SoundFile {name = "", title = "", tempo = "", notes = [Note {notename = "", octave = 0, timing = ""}], lyrics = [""], video = "", score = "", key = "", timeSignature = ""}

type CSVKeyValue = (T.Text, [T.Text])

notesToJSON :: T.Text -> Note
notesToJSON note = Note {notename = nameofnote, octave = noteoctave, timing = notetiming}
  where
    split  = T.breakOn "/" note
    notetiming = snd $ split
    nameofnote = T.filter (/= '`') (fst split)
    noteoctave
            | (length group) == 1 = 4
            | (group !! 0) == nameofnote = 4+(T.length (group !! 1))
            | otherwise = (4-(T.length (group !! 0)))
      where
        group = T.group (fst split)

convertToProfile :: SoundFile -> CSVKeyValue -> SoundFile
convertToProfile soundfile ("name", val) = soundfile {name = (val !! 0)}
convertToProfile soundfile ("title", val) = soundfile {title = (val !! 0)}
convertToProfile soundfile ("tempo", val) = soundfile {tempo = (val !! 0)}
convertToProfile soundfile ("notes", val) = soundfile {notes = (map (notesToJSON) val)}
convertToProfile soundfile ("lyrics", val) = soundfile {lyrics = (val)}
convertToProfile soundfile ("video", val) = soundfile {video = (val !! 0)}
convertToProfile soundfile ("score", val) = soundfile {score = (val !! 0)}
convertToProfile soundfile ("key", val) = soundfile {key = (val !! 0)}
convertToProfile soundfile ("time signature", val) = soundfile {timeSignature = (val !! 0)}
convertToProfile soundfile _ = soundfile

toKeyValue :: T.Text -> CSVKeyValue
toKeyValue line = (key, value)
  where
    ss = T.splitOn "," line
    key = head ss
    value = tail ss

trimTrailingCommas :: T.Text -> T.Text
trimTrailingCommas line = T.dropWhileEnd (== ',') line


convert file = LazyT.toStrict $ encodeToLazyText $ foldl convertToProfile emptyProfile keyValue
  where
    keyValue = Prelude.map (toKeyValue . trimTrailingCommas) file
