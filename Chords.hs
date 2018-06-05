module Chords where

import Scales
import Control.Lens
import Euterpea

data Chord = Chord
  {
    chordRoot  :: TaggedNote,
    chordNotes :: PitchCollection
  } deriving (Eq, Show, Read)

ppChord :: Chord -> String
ppChord c = concatMap ppNote (chordNotes c)

chordDegsFrom :: AbsPitch -> Degree -> Mode -> Chord
chordDegsFrom myRoot ext mode
  | (ext == 6)  = Chord
                  {
                    chordNotes = getNotes[1, 3, 5, 6],
                    chordRoot  = TaggedNote
                    {
                      getAbs = myRoot,
                      degree = 1
                    }
                  }     
  | (ext <= 13) = Chord
                  {
                    chordNotes = getNotes [1, 3 .. ext],
                    chordRoot = TaggedNote
                           {
                             getAbs = myRoot,
                             degree = 1
                           }
                  }
  | otherwise   = error "You tried to use an upper extension above the 13th. Why did you do that?" where
      getNotes degs = map (nthDegree mode myRoot) degs
