{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyT
import Data.Aeson.Text
import Data.Aeson
import GHC.Generics
import Text.Regex.TDFA

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
            | (length group) == 0 = 0
            | (length group) == 1 = 4
            | (group !! 0) == nameofnote = 4+(T.length (group !! 1))
            | otherwise = (4-(T.length (group !! 0)))
      where
        group = T.group (fst split)


-- Deterministic Finite Automata
splitEsc :: (Foldable t1, Eq t) => t -> t -> t1 t -> [[t]]
splitEsc sep esc = reverse . map reverse . snd . foldl process (0, [[]])
  where process (st, r:rs) ch
                            | st == 0 && ch == esc               = (1,      r:rs)
                            | st == 1 && ch == sep               = (1, (ch:r):rs)
                            | st == 0 && ch == sep               = (0,   []:r:rs)
                            | st == 1 && ch /= esc               = (1, (ch:r):rs)
                            | st == 1 && ch == esc               = (0,      r:rs)
                            | otherwise                          = (0, (ch:r):rs)



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
    ss = splitEsc ',' '\"' (T.unpack line)
    key = T.pack (head ss)
    value
        | tail ss == [] = ["null"]
        | otherwise = map (T.pack) (tail ss)

trimTrailingCommas :: T.Text -> T.Text
trimTrailingCommas line = T.dropWhileEnd (== ',') line


convert file = LazyT.toStrict $ encodeToLazyText $ foldl convertToProfile emptyProfile keyValue
  where
    keyValue = Prelude.map (toKeyValue) $ filter (/= "") $ Prelude.map (trimTrailingCommas) file
