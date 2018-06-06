{-# LANGUAGE TemplateHaskell #-}

module Chords where

import Scales
import Control.Lens
import Euterpea

data Chord = Chord
  {
    _chordRoot  :: PitchClass,
    _chordNotes :: PitchCollection
  } deriving (Eq, Show, Read)

makeLenses ''Chord

ppChord :: Chord -> String
ppChord c = concatMap ppNote (_chordNotes c)

chordDegsFrom :: AbsPitch -> Degree -> Mode -> Chord
chordDegsFrom myRoot ext mode
  | (ext == 6)  = Chord
                  {
                    _chordNotes = getNotes[1, 3, 5, 6],
                    _chordRoot  = fst $ pitch myRoot
                  }     
  | (ext <= 13) = Chord
                  {
                    _chordNotes = getNotes [1, 3 .. ext],
                    _chordRoot  = fst $ pitch myRoot
                  }
  | otherwise   = error "You tried to use an upper extension above the 13th. Why did you do that?" where
      getNotes degs = map (nthDegree mode myRoot) degs
